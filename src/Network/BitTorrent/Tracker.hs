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
{-# LANGUAGE RecordWildCards   #-}
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

         -- * Scrape
       , ScrapeInfo(..), Scrape
       , scrapeURL
       , scrape, scrapeOne
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.BoundedChan as BC
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import Data.BEncode
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char
import           Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.IORef
import           Data.Text (Text)

import Network
import Network.HTTP
import Network.URI

import Data.Torrent
import Network.BitTorrent.Sessions.Types
import Network.BitTorrent.Peer
import Network.BitTorrent.Tracker.Protocol

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
waitInterval se @ TSession {..} = do
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

{-----------------------------------------------------------------------
    Scrape
-----------------------------------------------------------------------}

-- | Scrape info about a set of torrents.
type Scrape = Map InfoHash ScrapeInfo

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
