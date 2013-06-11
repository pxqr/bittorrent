-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
--   This module provides high level API for peer->tracker
--   communication.
--
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker
       ( module Network.BitTorrent.Tracker.Scrape

       , withTracker, completedReq

         -- * Progress
       , Progress(..), startProgress

         -- * Connection
       , TConnection(..), tconnection

         -- * Session
       , TSession, getPeerList, getProgress, waitInterval

         -- * Re-export
       , defaultPorts
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Torrent
import Network
import Network.URI

import Network.BitTorrent.Internal
import Network.BitTorrent.Peer
import Network.BitTorrent.Tracker.Protocol
import Network.BitTorrent.Tracker.Scrape


-- | 'TConnection' (shorthand for Tracker session) combines tracker request
--   fields neccessary for tracker, torrent and client identification.
--
--   This data is considered as static within one session.
--
data TConnection = TConnection {
    tconnAnnounce :: URI        -- ^ Announce URL.
  , tconnInfoHash :: InfoHash   -- ^ Hash of info part of current .torrent file.
  , tconnPeerID   :: PeerID     -- ^ Client peer ID.
  , tconnPort     :: PortNumber -- ^ The port number the client is listenning on.
  } deriving Show

tconnection :: Torrent -> PeerID -> PortNumber -> TConnection
tconnection t = TConnection (tAnnounce t) (tInfoHash t)


-- | used to avoid boilerplate; do NOT export me
genericReq :: TConnection -> Progress -> TRequest
genericReq ses pr =   TRequest {
    reqAnnounce   = tconnAnnounce ses
  , reqInfoHash   = tconnInfoHash ses
  , reqPeerID     = tconnPeerID   ses
  , reqPort       = tconnPort     ses

  , reqUploaded   = prUploaded   pr
  , reqDownloaded = prDownloaded pr
  , reqLeft       = prLeft       pr

  , reqIP         = Nothing
  , reqNumWant    = Nothing
  , reqEvent      = Nothing
  }


-- | The first request to the tracker that should be created is 'startedReq'.
--   It includes necessary 'Started' event field.
--
startedReq :: TConnection -> Progress -> TRequest
startedReq ses pr = (genericReq ses pr) {
    reqIP         = Nothing
  , reqNumWant    = Just defaultNumWant
  , reqEvent      = Just Started
  }

-- | Regular request must be sent to keep track new peers and
--   notify tracker about current state of the client
--   so new peers could connect to the client.
--
regularReq :: Int -> TConnection -> Progress -> TRequest
regularReq numWant ses pr = (genericReq ses pr) {
    reqIP         = Nothing
  , reqNumWant    = Just numWant
  , reqEvent      = Nothing
  }

-- | Must be sent to the tracker if the client is shutting down
-- gracefully.
--
stoppedReq :: TConnection -> Progress -> TRequest
stoppedReq ses pr = (genericReq ses pr) {
    reqIP         = Nothing
  , reqNumWant    = Nothing
  , reqEvent      = Just Stopped
  }

-- | Must be sent to the tracker when the download completes.
-- However, must not be sent if the download was already 100%
-- complete.
--
completedReq :: TConnection -> Progress -> TRequest
completedReq ses pr = (genericReq ses pr) {
    reqIP         = Nothing
  , reqNumWant    = Nothing
  , reqEvent      = Just Completed
  }




data TSession = TSession {
    seProgress   :: TVar Progress
  , seInterval   :: IORef Int
  , sePeers      :: TVar [PeerAddr]
  }

newSession :: Progress -> Int -> [PeerAddr] -> IO TSession
newSession pr i ps = TSession <$> newTVarIO pr
                              <*> newIORef i
                              <*> newTVarIO ps

getPeerList :: TSession -> IO [PeerAddr]
getPeerList = readTVarIO . sePeers

getProgress :: TSession -> IO Progress
getProgress = readTVarIO . seProgress

waitInterval :: TSession -> IO ()
waitInterval = readIORef . seInterval >=> threadDelay

withTracker :: Progress -> TConnection -> (TSession -> IO a) -> IO a
withTracker initProgress conn action = bracket start end (action . fst)
  where
    start = do
      resp <- askTracker (startedReq conn initProgress)
      se   <- newSession initProgress (respInterval resp) (respPeers resp)
      tid  <- forkIO (syncSession se)
      return (se, tid)

    syncSession se @ TSession {..} = forever $ do
        waitInterval se
        pr   <- getProgress se
        resp <- tryJust isIOException $ do
                    askTracker (regularReq defaultNumWant conn pr)
        case resp of
          Right (OK {..}) -> do
            writeIORef seInterval respInterval
            atomically $ writeTVar sePeers respPeers
          _ -> return ()
      where
        isIOException :: IOException -> Maybe IOException
        isIOException = return

    end (se, tid) = do
      killThread tid
      pr <- getProgress se
      askTracker $ stoppedReq conn pr
