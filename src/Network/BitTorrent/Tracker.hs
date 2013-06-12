-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
--   This module provides high level API for peer->tracker
--   communication. Tracker is used to discover other peers in the
--   network.
--
--   By convention most trackers support another form of request,
--   which queries the state of a given torrent (or all torrents) that
--   the tracker is managing. This module also provides a way to
--   easily request scrape info for a particular torrent list.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker
       ( withTracker, completedReq

         -- * Connection
       , TConnection(..), tconnection

         -- * Session
       , TSession
       , getPeerAddr, getPeerList
       , getProgress, waitInterval

         -- * Re-export
       , defaultPorts

         -- * Scrape
       , ScrapeInfo(..), Scrape
       , scrapeURL
       , scrape, scrapeOne
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.BEncode
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.IORef

import Network
import Network.HTTP
import Network.URI

import Data.Torrent
import Network.BitTorrent.Internal
import Network.BitTorrent.Peer
import Network.BitTorrent.Tracker.Protocol


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


-- | The first request to the tracker that should be created is
--   'startedReq'. It includes necessary 'Started' event field.
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
  , sePeers      :: Chan PeerAddr
    -- TODO use something like 'TVar (Set PeerAddr)'
    -- otherwise we might get space leak
  }

newSession :: Progress -> Int -> [PeerAddr] -> IO TSession
newSession pr i ps = do
  chan <- newChan
  writeList2Chan chan ps
  TSession <$> newTVarIO pr
           <*> newIORef i
           <*> pure chan

getPeerAddr :: TSession -> IO PeerAddr
getPeerAddr = readChan . sePeers

getPeerList :: TSession -> IO [PeerAddr]
getPeerList = getChanContents . sePeers

getProgress :: TSession -> IO Progress
getProgress = readTVarIO . seProgress

sec :: Int
sec = 1000 * 1000

waitInterval :: TSession -> IO ()
waitInterval se @ TSession {..} = do
  delay <- readIORef seInterval
  print delay
  threadDelay (delay * sec)

withTracker :: Progress -> TConnection -> (TSession -> IO a) -> IO a
withTracker initProgress conn action = bracket start end (action . fst)
  where
    start = do
      resp <- askTracker (startedReq conn initProgress)
      print resp
      se   <- newSession initProgress (respInterval resp) (respPeers resp)
      tid  <- forkIO (return ()) -- (syncSession se)
      return (se, tid)

    syncSession se @ TSession {..} = forever $ do
        waitInterval se
        pr   <- getProgress se
        print "tracker req"
        resp <- tryJust isIOException $ do
                    askTracker (regularReq defaultNumWant conn pr)
        print "tracker resp"
        case resp of
          Right (ok @ OK {..}) -> do
            print ok
            writeIORef seInterval respInterval
            writeList2Chan sePeers respPeers
          _ -> return ()
      where
        isIOException :: IOException -> Maybe IOException
        isIOException = return

    end (se, tid) = do
      killThread tid
      pr <- getProgress se
      print  "stopping"
      leaveTracker $ stoppedReq conn pr



-- | Information about particular torrent.
data ScrapeInfo = ScrapeInfo {
    siComplete   :: Int
    -- ^ Number of seeders - peers with the entire file.
  , siDownloaded :: Int
    -- ^ Total number of times the tracker has registered a completion.
  , siIncomplete :: Int
    -- ^ Number of leechers.
  , siName       :: Maybe ByteString
    -- ^ Name of the torrent file, as specified by the "name"
    --   file in the info section of the .torrent file.
  } deriving (Show, Eq)

-- | Scrape info about a set of torrents.
type Scrape = Map InfoHash ScrapeInfo

instance BEncodable ScrapeInfo where
  toBEncode si = fromAssocs
    [ "complete"   -->  siComplete si
    , "downloaded" -->  siDownloaded si
    , "incomplete" -->  siIncomplete si
    , "name"       -->? siName si
    ]

  fromBEncode (BDict d) =
    ScrapeInfo <$> d >--  "complete"
               <*> d >--  "downloaded"
               <*> d >--  "incomplete"
               <*> d >--? "name"
  fromBEncode _ = decodingError "ScrapeInfo"

-- | Trying to convert /announce/ URL to /scrape/ URL. If 'scrapeURL'
--   gives 'Nothing' then tracker do not support scraping. The info hash
--   list is used to restrict the tracker's report to that particular
--   torrents. Note that scrapping of multiple torrents may not be
--   supported. (Even if scrapping convention is supported)
--
scrapeURL :: URI -> [InfoHash] -> Maybe URI
scrapeURL uri ihs = do
  newPath <- replace (BC.pack (uriPath uri))
  let newURI = uri { uriPath = BC.unpack newPath }
  return (foldl addHashToURI newURI ihs)
 where
    replace :: ByteString -> Maybe ByteString
    replace p
      | ps <- BC.splitWith (== '/') p
      , "announce" `B.isPrefixOf` last ps
      = let newSuff = "scrape" <> B.drop (B.length "announce") (last ps)
        in Just (B.intercalate "/" (init ps ++ [newSuff]))
      | otherwise = Nothing


-- | For each 'InfoHash' of torrents request scrape info from the tracker.
--   However if the info hash list is 'null', the tracker should list
--   all available torrents.
--   Note that the 'URI' should be /announce/ URI, not /scrape/ URI.
--
scrape :: URI                -- ^ Announce 'URI'.
       -> [InfoHash]         -- ^ Torrents to be scrapped.
       -> IO (Result Scrape) -- ^ 'ScrapeInfo' for each torrent.
scrape announce ihs
  | Just uri<- scrapeURL announce ihs = do
    rawResp  <- simpleHTTP (Request uri GET [] "")
    respBody <- getResponseBody rawResp
    return (decoded (BC.pack respBody))

  | otherwise = return (Left "Tracker do not support scraping")

-- | More particular version of 'scrape', just for one torrent.
--
scrapeOne :: URI                     -- ^ Announce 'URI'
          -> InfoHash                -- ^ Hash of the torrent info.
          -> IO (Result ScrapeInfo)  -- ^ 'ScrapeInfo' for the torrent.
scrapeOne uri ih = extract <$> scrape uri [ih]
  where
    extract (Right m)
      | Just s <- M.lookup ih m = Right s
      | otherwise = Left "unable to find info hash in response dict"
    extract (Left e) = Left e
