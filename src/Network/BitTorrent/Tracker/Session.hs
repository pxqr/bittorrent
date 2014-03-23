-- |
--   Copyright   :  (c) Sam Truzjan 2014
--   License     :  BSD
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Multitracker sessions.
--
{-# LANGUAGE TemplateHaskell #-}
module Network.BitTorrent.Tracker.Session
       ( -- * Session
         Session
       , newSession
       , closeSession
       , withSession

         -- * Client send notifications
       , Event (..)
       , notify
       , askPeers

         -- * Session state
         -- ** Status
       , Status (..)
       , getStatus

         -- ** Single tracker sessions
       , LastScrape (..)
       , TrackerEntry
       , trackerURI
       , trackerPeers
       , trackerScrape
       , getSessionState

         -- * Tracker Exchange
         -- | BEP28: <http://www.bittorrent.org/beps/bep_0028.html>
       , addTracker
       , removeTracker
       , getTrustedTrackers

         -- * Events
       , SessionEvent (..)
       , subscribe
       ) where

import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan.Split
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Default
import Data.Fixed
import Data.Foldable as F
import Data.IORef
import Data.List as L
import Data.Maybe
import Data.Time
import Data.Traversable
import Network.URI

import Data.Torrent.InfoHash
import Data.Torrent.JSON
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
  { -- | Count of leechers the tracker aware of.
    scrapeLeechers :: Maybe Int

    -- | Count of seeders the tracker aware of.
  , scrapeSeeders  :: Maybe Int
  } deriving (Show, Eq)

$(deriveJSON omitRecordPrefix ''LastScrape)

-- | Single tracker session.
data TrackerEntry = TrackerEntry
  { -- | Tracker announce URI.
    trackerURI    :: !URI

    -- | Used to notify 'Stopped' and 'Completed' events.
  , statusSent    :: !(Maybe Status)

    -- | Can be used to retrieve peer set.
  , trackerPeers  :: Cached [PeerAddr IP]

    -- | Can be used to show brief swarm stats in client GUI.
  , trackerScrape :: Cached LastScrape
  }

instance ToJSON TrackerEntry where
  toJSON TrackerEntry {..} = object
    [ "uri"    .= trackerURI
    , "peers"  .= trackerPeers
    , "scrape" .= trackerScrape
    ]

-- | Single tracker session with empty state.l
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
      { scrapeSeeders  = respComplete
      , scrapeLeechers = respIncomplete
      }

-- | Make announce request to specific tracker returning new state.
notifyTo :: Manager -> Session -> Event -> TrackerEntry -> IO TrackerEntry
notifyTo mgr s @ Session {..} event entry @ TrackerEntry {..} = do
  let shouldNotify = needNotify event statusSent
  mustNotify <- maybe (isExpired trackerPeers) return shouldNotify
  if not mustNotify
    then return entry
    else do
      let q = SAnnounceQuery sessionTopic def Nothing (Just event)
      res <- RPC.announce mgr trackerURI q
      when (statusSent == Nothing) $ do
        send sessionEvents (TrackerConfirmed trackerURI)
      send sessionEvents (AnnouncedTo trackerURI)
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
    sessionTopic    :: !InfoHash

    -- | Current status of this client is used to filter duplicated
    -- notifications, for e.g. we don't want to notify a tracker with
    -- ['Stopped', 'Stopped'], the last should be ignored.
  , sessionStatus   :: !(IORef Status)

    -- | A set of single-tracker sessions. Any request to a tracker
    -- must take a lock.
  , sessionTrackers :: !(MVar (TrackerList TrackerEntry))

  , sessionEvents   :: !(SendPort SessionEvent)
  }

-- | Create a new multitracker session in paused state. Tracker list
-- must contant only /trusted/ tracker uris. To start announcing
-- client presence use 'notify'.
newSession :: InfoHash -> TrackerList URI -> IO Session
newSession ih origUris = do
  urisList    <- shuffleTiers origUris
  statusRef   <- newIORef def
  entriesVar  <- newMVar (fmap nullEntry urisList)
  eventStream <- newSendPort
  return Session
    { sessionTopic    = ih
    , sessionStatus   = statusRef
    , sessionTrackers = entriesVar
    , sessionEvents   = eventStream
    }

-- | Release scarce resources associated with the given session. This
-- function block until all trackers tied with this peer notified with
-- 'Stopped' event.
closeSession :: Manager -> Session -> IO ()
closeSession m s @ Session {..} = do
  notify m s Stopped
  send sessionEvents SessionClosed

{-----------------------------------------------------------------------
--  Events
-----------------------------------------------------------------------}

data SessionEvent
  = TrackerAdded     URI
  | TrackerConfirmed URI
  | TrackerRemoved   URI
  | AnnouncedTo      URI
  | SessionClosed

subscribe :: Session -> IO (ReceivePort SessionEvent)
subscribe Session {..} = listen sessionEvents

{-----------------------------------------------------------------------
--  Operations
-----------------------------------------------------------------------}

-- | Normally you need to use 'Control.Monad.Trans.Resource.alloc'.
withSession :: Manager -> InfoHash -> TrackerList URI
            -> (Session -> IO ()) -> IO ()
withSession m ih uris = bracket (newSession ih uris) (closeSession m)

-- | Get last announced status. The only action can alter this status
-- is 'notify'.
getStatus :: Session -> IO Status
getStatus Session {..} = readIORef sessionStatus

getSessionState :: Session -> IO (TrackerList TrackerEntry)
getSessionState Session {..} = readMVar sessionTrackers

-- | Do we need to sent this event to a first working tracker or to
-- the all known good trackers?
allNotify :: Event -> Bool
allNotify Started   = False
allNotify Stopped   = True
allNotify Completed = True

notifyAll :: Manager -> Session -> Event -> IO ()
notifyAll mgr s @ Session {..} event = do
    modifyMVar_ sessionTrackers $
      (traversal (notifyTo mgr s event))
  where
    traversal
      | allNotify event = traverseAll
      |    otherwise    = traverseTiers

-- TODO send notifications to tracker periodically.
-- |
--
-- This function /may/ block until tracker query proceed.
notify :: Manager -> Session -> Event -> IO ()
notify mgr ses event = do
  prevStatus <- atomicModifyIORef (sessionStatus ses) $ \ s ->
                  (fromMaybe s (nextStatus event), s)
  when (needNotify event (Just prevStatus) == Just True) $ do
    notifyAll mgr ses event

-- TODO run announce if sesion have no peers
-- | The returned list of peers can have duplicates.
--   This function /may/ block. Use async if needed.
askPeers :: Manager -> Session -> IO [PeerAddr IP]
askPeers _mgr ses = do
  list    <- readMVar (sessionTrackers ses)
  L.concat <$> collect (tryTakeData . trackerPeers) list

collect :: (a -> IO (Maybe b)) -> TrackerList a -> IO [b]
collect f lst =(catMaybes . toList) <$> traverse f lst

--sourcePeers :: Session -> Source (PeerAddr IP)
--sourcePeers

{-----------------------------------------------------------------------
--  Tracker exchange
-----------------------------------------------------------------------}

-- Trackers discovered through this protocol SHOULD be treated with a
-- certain amount of suspicion. Since the source of a tracker exchange
-- message cannot be trusted, an implementation SHOULD have a lower
-- number of retries before giving up entirely.

addTracker :: Session -> URI -> IO ()
addTracker Session {..} uri = do
  undefined
  send sessionEvents (TrackerAdded uri)

removeTracker :: Session -> URI -> IO ()
removeTracker Session {..} uri = do
  undefined
  send sessionEvents (TrackerRemoved uri)

-- Also, as specified under the definitions section, a tracker that
-- has not worked should never be propagated to other peers over the
-- tracker exchange protocol.

-- | Return all known trackers.
getTrackers :: Session -> IO [URI]
getTrackers = undefined

-- | Return trackers from torrent file and
getTrustedTrackers :: Session -> IO [URI]
getTrustedTrackers = undefined
