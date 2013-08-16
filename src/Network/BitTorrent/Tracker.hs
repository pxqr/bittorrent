-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
--   This module provides high level API for peer -> tracker
--   communication. Tracker is used to discover other peers in the
--   network using torrent info hash.
--
{-# LANGUAGE TemplateHaskell   #-}
module Network.BitTorrent.Tracker
       ( withTracker, completedReq

         -- * Connection
       , TConnection(..), tconnection

         -- * Session
       , TSession
       , getPeerAddr
       , getProgress, waitInterval

         -- * Re-export
       , defaultPorts
       , ScrapeInfo
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.BoundedChan as BC
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import           Data.List as L
import           Data.IORef

import Network
import Network.URI

import Data.Torrent.Metainfo
import Network.BitTorrent.Peer
import Network.BitTorrent.Sessions.Types
import Network.BitTorrent.Tracker.Protocol
import Network.BitTorrent.Tracker.HTTP

{-----------------------------------------------------------------------
    Tracker connection
-----------------------------------------------------------------------}

-- | 'TConnection' (shorthand for Tracker session) combines tracker
-- request fields neccessary for tracker, torrent and client
-- identification.
--
--   This data is considered as static within one session.
--
data TConnection = TConnection {
    tconnAnnounce :: URI        -- ^ Announce URL.
  , tconnInfoHash :: InfoHash   -- ^ Hash of info part of current .torrent file.
  , tconnPeerId   :: PeerId     -- ^ Client peer ID.
  , tconnPort     :: PortNumber -- ^ The port number the client is listenning on.
  } deriving Show

-- TODO tconnection :: SwarmSession -> TConnection
tconnection :: Torrent -> PeerId -> PortNumber -> TConnection
tconnection t = TConnection (tAnnounce t) (tInfoHash t)

-- | used to avoid boilerplate; do NOT export me
genericReq :: TConnection -> Progress -> AnnounceQuery
genericReq ses pr =   AnnounceQuery {
    reqInfoHash   = tconnInfoHash ses
  , reqPeerId     = tconnPeerId   ses
  , reqPort       = tconnPort     ses

  , reqUploaded   = _uploaded   pr
  , reqDownloaded = _downloaded pr
  , reqLeft       = _left       pr

  , reqIP         = Nothing
  , reqNumWant    = Nothing
  , reqEvent      = Nothing
  }

-- | The first request to the tracker that should be created is
--   'startedReq'. It includes necessary 'Started' event field.
--
startedReq :: TConnection -> Progress -> AnnounceQuery
startedReq ses pr = (genericReq ses pr) {
    reqIP         = Nothing
  , reqNumWant    = Just defaultNumWant
  , reqEvent      = Just Started
  }

-- | Regular request must be sent to keep track new peers and
--   notify tracker about current state of the client
--   so new peers could connect to the client.
--
regularReq :: Int -> TConnection -> Progress -> AnnounceQuery
regularReq numWant ses pr = (genericReq ses pr) {
    reqIP         = Nothing
  , reqNumWant    = Just numWant
  , reqEvent      = Nothing
  }

-- | Must be sent to the tracker if the client is shutting down
-- gracefully.
--
stoppedReq :: TConnection -> Progress -> AnnounceQuery
stoppedReq ses pr = (genericReq ses pr) {
    reqIP         = Nothing
  , reqNumWant    = Nothing
  , reqEvent      = Just Stopped
  }

-- | Must be sent to the tracker when the download completes.
-- However, must not be sent if the download was already 100%
-- complete.
--
completedReq :: TConnection -> Progress -> AnnounceQuery
completedReq ses pr = (genericReq ses pr) {
    reqIP         = Nothing
  , reqNumWant    = Nothing
  , reqEvent      = Just Completed
  }

{-----------------------------------------------------------------------
    Tracker session
-----------------------------------------------------------------------}

{-  Why use BoundedChan?

Because most times we need just a list of peer at the start and all
the rest time we will take little by little. On the other hand tracker
will give us some constant count of peers and channel will grow with
time. To avoid space leaks and long lists of peers (which we don't
need) we use bounded chaan.

   Chan size.

Should be at least (count_of_workers * 2) to accumulate long enough
peer list.

  Order of peers in chan.

Old peers in head, new ones in tail. Old peers should be used in the
first place because by statistics they are most likely will present in
network a long time than a new.

-}

type TimeInterval = Int

data TSession = TSession {
  -- TODO synchonize progress with client session
    seProgress   :: TVar Progress
  , seInterval   :: IORef TimeInterval
  , sePeers      :: BoundedChan PeerAddr
  }

type PeerCount = Int

defaultChanSize :: PeerCount
defaultChanSize = defaultNumWant * 2

getPeerAddr :: TSession -> IO PeerAddr
getPeerAddr = BC.readChan . sePeers

getProgress :: TSession -> IO Progress
getProgress = readTVarIO . seProgress

newSession :: PeerCount -> Progress -> TimeInterval -> [PeerAddr]
           -> IO TSession
newSession chanSize pr i ps
  | chanSize < 1
  = throwIO $ userError "size of chan should be more that 1"

  | otherwise = do
    chan <- newBoundedChan chanSize

    -- if length of the "ps" is more than the "chanSize" we will block
    -- forever; to avoid this we remove excessive peers
    let ps' = take chanSize ps
    BC.writeList2Chan chan ps'

    TSession <$> newTVarIO pr
             <*> newIORef i
             <*> pure chan

waitInterval :: TSession -> IO ()
waitInterval TSession {..} = do
    delay <- readIORef seInterval
    threadDelay (delay * sec)
  where
    sec = 1000 * 1000 :: Int

withTracker :: Progress -> TConnection -> (TSession -> IO a) -> IO a
withTracker initProgress conn action = bracket start end (action . fst)
  where
    start = do
      resp <- askTracker (tconnAnnounce conn) (startedReq conn initProgress)
      se   <- newSession defaultChanSize initProgress
                         (respInterval resp) (respPeers resp)

      tid  <- forkIO (syncSession se)
      return (se, tid)

    syncSession se @ TSession {..} = forever $ do
        waitInterval se
        pr   <- getProgress se
        resp <- tryJust isIOException $ do
                    askTracker (tconnAnnounce conn) (regularReq defaultNumWant conn pr)
        case resp of
          Right (AnnounceInfo {..}) -> do
            writeIORef seInterval respInterval

            -- we rely on the fact that union on lists is not
            -- commutative: this implements the heuristic "old peers
            -- in head"
            old <- BC.getChanContents sePeers
            let combined = L.union old respPeers
            BC.writeList2Chan sePeers combined

          _ -> return ()
      where
        isIOException :: IOException -> Maybe IOException
        isIOException = return

    end (se, tid) = do
      killThread tid
      pr <- getProgress se
      leaveTracker (tconnAnnounce conn) (stoppedReq conn pr)
