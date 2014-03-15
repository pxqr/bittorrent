-- |
--   Copyright   :  (c) Sam Truzjan 2014
--   License     :  BSD
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Multitracker sessions.
--
module Network.BitTorrent.Tracker.Session
       ( -- * Session
         Session
       , newSession
       , closeSession

         -- * Trackers
       , notify
       , askPeers

         -- * Tracker Exchange
         -- | BEP28: <http://www.bittorrent.org/beps/bep_0028.html>
       , addTracker
       , removeTracker
       , getTrackers
       , getTrustedTrackers
       ) where

import Control.Applicative
import Control.Concurrent
import Data.Default
import Data.Fixed
import Data.Foldable
import Data.List as L
import Data.Maybe
import Data.Time
import Data.Traversable
import Network.URI

import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.Tracker.Cache
import Network.BitTorrent.Tracker.List
import Network.BitTorrent.Tracker.Message
import Network.BitTorrent.Tracker.RPC as RPC

{-----------------------------------------------------------------------
--  Tracker entry
-----------------------------------------------------------------------}

data LastScrape = LastScrape
  { leechersCount :: Maybe Int
  , seedersCount  :: Maybe Int
  } deriving (Show, Eq)

instance Default LastScrape where
  def = LastScrape Nothing Nothing


data Status
  = Running
  | Paused
    deriving (Show, Eq)

instance Default Status where
  def = Paused

nextStatus :: Maybe Event -> Status
nextStatus Nothing          = Running
nextStatus (Just Started  ) = Running
nextStatus (Just Stopped  ) = Paused
nextStatus (Just Completed) = Running

needNotify :: Maybe Event -> Maybe Status -> Bool
-- we always send _regular_ announce requests (for e.g. to get more peers);
needNotify  Nothing          _             = True
needNotify (Just Started)    Nothing       = True
needNotify (Just Stopped)    Nothing       = False
needNotify (Just Completed)  Nothing       = False
needNotify  Nothing         (Just Running) = True
needNotify  Nothing         (Just Paused ) = True

-- | Do we need to sent this event to a first working tracker or to
-- the all known good trackers?
allNotify :: Maybe Event -> Bool
allNotify  Nothing         = False
allNotify (Just Started)   = False
allNotify (Just Stopped)   = True
allNotify (Just Completed) = True

-- | Single tracker session.
data TrackerEntry = TrackerEntry
  { -- | Tracker announce URI.
    trackerURI    :: !URI

    -- | Used to notify 'Stopped' and 'Completed' events.
  , statusSent    :: !(Maybe Status)

    -- |
  , peersCache    :: Cached [PeerAddr IP]

    -- | May be used to show brief swarm stats in client GUI.
  , scrapeCache   :: Cached LastScrape
  }

nullEntry :: URI -> TrackerEntry
nullEntry uri = TrackerEntry uri Nothing def def

{-----------------------------------------------------------------------
--  Multitracker Session
-----------------------------------------------------------------------}

-- | Multitracker session.
data Session = Session
  { infohash  :: !InfoHash
  , currentStatus :: !(MVar Status)
  , trackers  :: !(MVar (TrackerList TrackerEntry))
  }

-- Just Started
newSession :: InfoHash -> TrackerList URI -> IO Session
newSession ih origUris = do
  uris    <- shuffleTiers origUris
  status  <- newMVar def
  entries <- newMVar (fmap nullEntry uris)
  return (Session ih status entries)

-- Just Stopped
closeSession :: Session -> IO ()
closeSession _ = return ()

seconds :: Int -> NominalDiffTime
seconds n = realToFrac (toEnum n :: Uni)

cachePeers :: AnnounceInfo -> IO (Cached [PeerAddr IP])
cachePeers AnnounceInfo {..} =
  newCached (seconds respInterval)
            (seconds (fromMaybe respInterval respMinInterval))
            (getPeerList respPeers)

cacheScrape :: AnnounceInfo -> IO (Cached LastScrape)
cacheScrape AnnounceInfo {..} =
  newCached (seconds respInterval)
            (seconds (fromMaybe respInterval respMinInterval))
    LastScrape
      { seedersCount  = respComplete
      , leechersCount = respIncomplete
      }

announceAll :: Manager -> Session -> Maybe Event -> IO ()
announceAll mgr Session {..} mevent = do
    modifyMVar_ trackers (traversal announceTo)
  where
    traversal
      | allNotify mevent = traverseAll
      |    otherwise     = traverseTiers

    announceTo entry @ TrackerEntry {..}
      | mevent `needNotify` statusSent = do
        let q = SAnnounceQuery infohash def Nothing mevent
        res <- RPC.announce mgr trackerURI q
        TrackerEntry trackerURI (Just (nextStatus mevent))
                     <$> cachePeers res <*> cacheScrape res
      | otherwise = return entry

-- TODO send notifications to tracker periodically.
-- |
--
-- This function /may/ block until tracker query proceed.
notify :: Manager -> Session -> Event -> IO ()
notify mgr ses event = announceAll mgr ses (Just event)

-- TODO fork thread for reannounces
-- |
announce :: Manager -> Session -> IO ()
announce mgr ses = announceAll mgr ses Nothing

-- TODO run announce if sesion have no peers
-- | This function /may/ block. Use async if needed.
askPeers :: Manager -> Session -> IO [PeerAddr IP]
askPeers mgr ses = do
  list    <- readMVar (trackers ses)
  L.concat <$> collect (tryTakeData . peersCache) list

collect :: (a -> IO (Maybe b)) -> TrackerList a -> IO [b]
collect f lst =(catMaybes . toList) <$> traverse f lst

--sourcePeers :: Session -> Source (PeerAddr IP)
--sourcePeers

{-----------------------------------------------------------------------
--  State query
-----------------------------------------------------------------------}

data TrackerInfo = TrackerInfo
  {
  }

--instance ToJSON TrackerInfo where
--  toJSON = undefined

--getSessionState :: Session -> IO (TrackerList TrackerInfo)
--getSessionState = undefined

{-----------------------------------------------------------------------
--  Tracker exchange
-----------------------------------------------------------------------}

-- Trackers discovered through this protocol SHOULD be treated with a
-- certain amount of suspicion. Since the source of a tracker exchange
-- message cannot be trusted, an implementation SHOULD have a lower
-- number of retries before giving up entirely.

addTracker :: Session -> URI -> IO ()
addTracker = undefined

removeTracker :: Session -> URI -> IO ()
removeTracker = undefined

-- Also, as specified under the definitions section, a tracker that
-- has not worked should never be propagated to other peers over the
-- tracker exchange protocol.

-- | Return all known trackers.
getTrackers :: Session -> IO [URI]
getTrackers = undefined

-- | Return trackers from torrent file and
getTrustedTrackers :: Session -> IO [URI]
getTrustedTrackers = undefined
