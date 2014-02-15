-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Tracker WAI application.
--
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker.Wai
       ( -- * Configuration
         TrackerSettings (..)

         -- * Tracker
       , Tracker
       , newTracker
       , closeTracker
       , withTracker

         -- * Application
       , tracker
       ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.BEncode as BE
import Data.Default
import Data.HashMap.Strict as HM
import Data.List as L
import Data.Maybe
import Network.HTTP.Types
import Network.Wai

import Data.Torrent.InfoHash
import Data.Torrent.Progress
import Network.BitTorrent.Tracker.Message


-- | Various configuration settings used to generate tracker response.
data TrackerSettings = TrackerSettings
  { announcePath          :: !RawPath
  , scrapePath            :: !RawPath

    -- | If peer did not specified the "numwant" then this value is
    -- used.
  , defNumWant            :: {-# UNPACK #-} !Int

    -- | If peer specified too big numwant value.
  , maxNumWant            :: {-# UNPACK #-} !Int

    -- | Recommended time interval to wait between regular announce
    -- requests.
  , reannounceInterval    :: {-# UNPACK #-} !Int

    -- | Minimum time interval to wait between regular announce
    -- requests.
  , reannounceMinInterval :: !(Maybe Int)

    -- | Whether to send count of seeders.
  , completePeers         :: !Bool

    -- | Whether to send count of leechers.
  , incompletePeers       :: !Bool

    -- | Do not send peer id in response. Peer can override this value
    -- by setting "no_peer_id" to 0 or 1.
  , noPeerId              :: !Bool

    -- | Whether to send compact peer list. Peer can override this
    -- value by setting "compact" to 0 or 1.
  , compactPeerList       :: !Bool
  } deriving (Show, Read, Eq)

-- | Conservative tracker settings compatible with any client.
instance Default TrackerSettings where
  def = TrackerSettings
    { announcePath          = defaultAnnouncePath
    , scrapePath            = defaultScrapePath
    , defNumWant            = defaultNumWant
    , maxNumWant            = defaultMaxNumWant
    , reannounceInterval    = defaultReannounceInterval
    , reannounceMinInterval = Nothing
    , compactPeerList       = False
    , completePeers         = False
    , incompletePeers       = False
    , noPeerId              = False
    }

{-----------------------------------------------------------------------
--  Swarm
-----------------------------------------------------------------------}

type PeerSet = [()]

data Swarm = Swarm
  { leechers   :: !PeerSet
  , seeders    :: !PeerSet
  , downloaded :: {-# UNPACK #-} !Int
  }

instance Default Swarm where
  def = Swarm
    { leechers   = []
    , seeders    = []
    , downloaded = 0
    }
{-
started :: PeerInfo -> Swarm -> Swarm
started info Swarm {..} = Swarm
  { leechers   = insert info leechers
  , seeders    = delete info seeders
  , downloaded = downloaded
  }

regular :: PeerInfo -> Swarm -> Swarm
regular info Swarm {..} = undefined

stopped :: PeerInfo -> Swarm -> Swarm
stopped info Swarm {..} = Swarm
  { leechers   = delete info leechers
  , seeders    = delete info seeders
  , downloaded = downloaded
  }

completed :: PeerInfo -> Swarm -> Swarm
completed info Swarm {..} = Swarm
  { leechers   = delete info leechers
  , seeders    = insert info seeders
  , downloaded = succ downloaded
  }

event :: Maybe Event -> Swarm -> Swarm
event = undefined
-}
--peerList :: TrackerSettings -> Swarm -> PeerList IP
peerList TrackerSettings {..} Swarm {..} = undefined --envelope peers
  where
    envelope = if compactPeerList then CompactPeerList else PeerList
    peers    = []

announceInfo :: TrackerSettings -> Swarm -> AnnounceInfo
announceInfo settings @ TrackerSettings {..} swarm @ Swarm {..} = AnnounceInfo
  { respComplete    = Just (L.length seeders)
  , respIncomplete  = Just (L.length leechers)
  , respInterval    = reannounceInterval
  , respMinInterval = reannounceMinInterval
  , respPeers       = undefined -- peerList settings swarm
  , respWarning     = Nothing
  }

scrapeEntry :: Swarm -> ScrapeEntry
scrapeEntry Swarm {..} = ScrapeEntry
  { siComplete   = L.length seeders
  , siDownloaded = downloaded
  , siIncomplete = L.length leechers
  , siName       = Nothing
  }

{-----------------------------------------------------------------------
--  Tracker state
-----------------------------------------------------------------------}

type Table = HashMap InfoHash Swarm

withSwarm :: TVar Table -> InfoHash -> (Maybe Swarm -> STM (a, Swarm)) -> STM a
withSwarm tableRef infohash action = do
  table <- readTVar tableRef
  (res, swarm') <- action (HM.lookup infohash table)
  writeTVar tableRef (HM.insert infohash swarm' table)
  return res

scrapeInfo :: ScrapeQuery -> Table -> [ScrapeEntry]
scrapeInfo query table = do
  infohash <- query
  swarm    <- maybeToList $ HM.lookup infohash table
  return    $ scrapeEntry swarm

data TrackerState = TrackerState
  { swarms :: !(TVar Table)
  }

newState :: IO TrackerState
newState = TrackerState <$> newTVarIO HM.empty

data Tracker = Tracker
  { options :: !TrackerSettings
  , state   :: !TrackerState
  }

newTracker :: TrackerSettings -> IO Tracker
newTracker opts = Tracker opts <$> newState

closeTracker :: Tracker -> IO ()
closeTracker _ = return ()

withTracker :: TrackerSettings -> (Tracker -> IO a) -> IO a
withTracker opts = bracket (newTracker opts) closeTracker

{-----------------------------------------------------------------------
--  Handlers
-----------------------------------------------------------------------}

getAnnounceR :: Tracker -> AnnounceRequest -> ResourceT IO AnnounceInfo
getAnnounceR Tracker {..} AnnounceRequest {..} = do
  return undefined
{-
  atomically $ do
    withSwarm (swarms state) (reqInfoHash announceQuery) $ \ mswarm ->
      case mswarm of
        Nothing -> return undefined
        Just s  -> return undefined
-}
getScrapeR :: Tracker -> ScrapeQuery -> ResourceT IO ScrapeInfo
getScrapeR Tracker {..} query = do
  table <- liftIO $ readTVarIO (swarms state)
  return $ undefined $ scrapeInfo query table

{-----------------------------------------------------------------------
--  Routing
-----------------------------------------------------------------------}

announceResponse :: AnnounceInfo -> Response
announceResponse info = responseLBS ok200 headers $ BE.encode info
  where
    headers = [(hContentType, announceType)]

scrapeResponse :: ScrapeInfo -> Response
scrapeResponse info = responseLBS ok200 headers $ BE.encode info
  where
    headers = [(hContentType, scrapeType)]

-- content-type: "text/plain"!
tracker :: Tracker -> Application
tracker t @ (Tracker TrackerSettings {..} _) Request {..}
  | requestMethod /= methodGet
  = return $ responseLBS methodNotAllowed405 [] ""

  | rawPathInfo == announcePath = do
    case parseAnnounceRequest $ queryToSimpleQuery queryString of
      Right query -> announceResponse <$> getAnnounceR t query
      Left  msg   -> return $ responseLBS (parseFailureStatus msg) [] ""

  | rawPathInfo == scrapePath   = do
    case Right $ parseScrapeQuery $ queryToSimpleQuery queryString of -- TODO
      Right query -> scrapeResponse <$> getScrapeR t query
      Left  msg   -> return $ responseLBS badRequest400 [] ""

  |     otherwise               = undefined --badPath
