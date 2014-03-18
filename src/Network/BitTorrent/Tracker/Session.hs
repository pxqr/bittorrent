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
       , withSession

         -- * Query
       , Status (..)
       , getStatus
       , askPeers

         -- * Events
       , Event (..)
       , notify

         -- * Tracker Exchange
         -- | BEP28: <http://www.bittorrent.org/beps/bep_0028.html>
       , addTracker
       , removeTracker
       , getTrustedTrackers
       ) where

import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.Default
import Data.Fixed
import Data.Foldable
import Data.IORef
import Data.List as L
import Data.Maybe
import Data.Time
import Data.Traversable
import Network.URI

import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.Internal.Cache
import Network.BitTorrent.Tracker.List
import Network.BitTorrent.Tracker.Message
import Network.BitTorrent.Tracker.RPC as RPC

{-----------------------------------------------------------------------
--  Single tracker session
-----------------------------------------------------------------------}

-- | Status of this client.
data Status
  = Running -- ^ This client is announced and listenning for incoming
            -- connections.
  | Paused  -- ^ This client does not expecting incoming connections.
    deriving (Show, Eq, Bounded, Enum)

-- | Client starting in the paused state.
instance Default Status where
  def = Paused

-- | Tracker session starts with scrape unknown.
instance Default LastScrape where
  def = LastScrape Nothing Nothing

data LastScrape = LastScrape
  { leechersCount :: Maybe Int
  , seedersCount  :: Maybe Int
  } deriving (Show, Eq)

-- | Single tracker session.
data TrackerEntry = TrackerEntry
  { -- | Tracker announce URI.
    trackerURI    :: !URI

    -- | Used to notify 'Stopped' and 'Completed' events.
  , statusSent    :: !(Maybe Status)

    -- | Can be used to retrieve peer set.
  , peersCache    :: Cached [PeerAddr IP]

    -- | Can be used to show brief swarm stats in client GUI.
  , scrapeCache   :: Cached LastScrape
  }

-- | Single tracker session with empty state.
nullEntry :: URI -> TrackerEntry
nullEntry uri = TrackerEntry uri Nothing def def

-- | Do we need to notify this /specific/ tracker?
needNotify :: Event -> Maybe Status -> Maybe Bool
needNotify Started    Nothing       = Just True
needNotify Stopped    Nothing       = Just False
needNotify Completed  Nothing       = Just False
needNotify Started   (Just Running) = Nothing
needNotify Stopped   (Just Running) = Just True
needNotify Completed (Just Running) = Just True
needNotify Started   (Just Paused ) = Just True
needNotify Stopped   (Just Paused ) = Just False
needNotify Completed (Just Paused ) = Just True

-- | Client status after event announce succeed.
nextStatus :: Event -> Maybe Status
nextStatus Started   = Just Running
nextStatus Stopped   = Just Paused
nextStatus Completed = Nothing -- must keep previous status

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

-- | Make announce request to specific tracker returning new state.
notifyTo :: Manager -> InfoHash -> Event
           -> TrackerEntry -> IO TrackerEntry
notifyTo mgr ih event entry @ TrackerEntry {..} = do

  let shouldNotify = needNotify event statusSent
  mustNotify <- maybe (isExpired peersCache) return shouldNotify
  if not mustNotify
    then return entry
    else do
      let q = SAnnounceQuery ih def Nothing (Just event)
      res <- RPC.announce mgr trackerURI q
      let status' = nextStatus event <|> statusSent
      TrackerEntry trackerURI status'
        <$> cachePeers res
        <*> cacheScrape res

{-----------------------------------------------------------------------
--  Multitracker Session
-----------------------------------------------------------------------}

-- | Multitracker session.
data Session = Session
  { -- | Infohash to announce at each 'announce' request.
    infohash  :: !InfoHash

    -- | Status of this client is used to filter duplicated
    -- notifications, for e.g. we don't want to notify a tracker with
    -- ['Stopped', 'Stopped'], the last should be ignored.
  , currentStatus :: !(IORef Status)

    -- | A set of single-tracker sessions. Any request to a tracker
    -- must take a lock.
  , trackers  :: !(MVar (TrackerList TrackerEntry))
  }

-- | Create a new multitracker session in paused state. Tracker list
-- must contant only /trusted/ tracker uris. To start announcing
-- client presence use 'notify'.
newSession :: InfoHash -> TrackerList URI -> IO Session
newSession ih origUris = do
  uris    <- shuffleTiers origUris
  status  <- newIORef def
  entries <- newMVar (fmap nullEntry uris)
  return (Session ih status entries)

-- | Release scarce resources associated with the given session.
closeSession :: Session -> IO ()
closeSession _ = return ()

-- | Normally you need to use 'Control.Monad.Trans.Resource.alloc'.
withSession :: InfoHash -> TrackerList URI -> (Session -> IO ()) -> IO ()
withSession ih uris = bracket (newSession ih uris) closeSession

-- | Get last announced status. The only action can alter this status
-- is 'notify'.
getStatus :: Session -> IO Status
getStatus Session {..} = readIORef currentStatus

-- | Do we need to sent this event to a first working tracker or to
-- the all known good trackers?
allNotify :: Event -> Bool
allNotify Started   = False
allNotify Stopped   = True
allNotify Completed = True

notifyAll :: Manager -> Session -> Event -> IO ()
notifyAll mgr Session {..} event = do
    modifyMVar_ trackers (traversal (notifyTo mgr infohash event))
  where
    traversal
      | allNotify event = traverseAll
      |    otherwise     = traverseTiers

-- TODO send notifications to tracker periodically.
-- |
--
-- This function /may/ block until tracker query proceed.
notify :: Manager -> Session -> Event -> IO ()
notify mgr ses event = do
  prevStatus <- atomicModifyIORef (currentStatus ses) $ \ s ->
                  (fromMaybe s (nextStatus event), s)
  when (needNotify event (Just prevStatus) == Just True) $ do
    notifyAll mgr ses event

-- TODO run announce if sesion have no peers
-- | The returned list of peers can have duplicates.
--   This function /may/ block. Use async if needed.
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
