module Network.BitTorrent.Client.Handle
       ( -- * Handle
         Handle

         -- * Initialization
       , openTorrent
       , openMagnet
       , closeHandle

         -- * Control
       , start
       , pause
       , stop

         -- * Query
       , getHandle
       , HandleState
       , getState
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Default
import Data.List as L
import Data.HashMap.Strict as HM

import Data.Torrent
import Data.Torrent.InfoHash
import Data.Torrent.Magnet
import Network.BitTorrent.Client.Types
import Network.BitTorrent.DHT      as DHT
import Network.BitTorrent.Exchange as Exchange
import Network.BitTorrent.Tracker  as Tracker

{-----------------------------------------------------------------------
--  Safe handle set manupulation
-----------------------------------------------------------------------}

-- | Guarantees that we newer allocate the same handle twice.
allocHandle :: InfoHash -> BitTorrent Handle -> BitTorrent Handle
allocHandle ih m = do
  c @ Client {..} <- getClient
  liftIO $ modifyMVar clientTorrents $ \ handles -> do
    case HM.lookup ih handles of
      Just h  -> return (handles, h)
      Nothing -> do
        h <- runBitTorrent c m
        return (HM.insert ih h handles, h)

-- |
freeHandle :: InfoHash -> BitTorrent () -> BitTorrent ()
freeHandle ih finalizer = do
  c @ Client {..} <- getClient
  liftIO $ modifyMVar_ clientTorrents $ \ handles -> do
    case HM.lookup ih handles of
      Nothing -> return handles
      Just _  -> do
        runBitTorrent c finalizer
        return (HM.delete ih handles)

-- |
lookupHandle :: InfoHash -> BitTorrent (Maybe Handle)
lookupHandle ih = do
  Client {..} <- getClient
  handles     <- liftIO $ readMVar clientTorrents
  return (HM.lookup ih handles)

{-----------------------------------------------------------------------
--  Initialization
-----------------------------------------------------------------------}

-- | Open a torrent in 'stop'ed state. Use 'nullTorrent' to open
-- handle from 'InfoDict'. This operation do not block.
openTorrent :: FilePath -> Torrent -> BitTorrent Handle
openTorrent rootPath t @ Torrent {..} = do
  let ih = idInfoHash tInfoDict
  allocHandle ih $ do
    c @ Client {..} <- getClient
    tses <- liftIO $ Tracker.newSession ih (trackerList t)
    eses <- liftIO $ Exchange.newSession clientLogger (externalAddr c)
                       rootPath tInfoDict
    return $ Handle
      { handleTopic    = ih
      , handlePrivate  = idPrivate tInfoDict
      , handleTrackers = tses
      , handleExchange = eses
      }

-- | Use 'nullMagnet' to open handle from 'InfoHash'.
openMagnet :: FilePath -> Magnet -> BitTorrent Handle
openMagnet rootPath uri @ Magnet {..} = do
  allocHandle exactTopic $ do
    c @ Client {..} <- getClient
    tses <- liftIO $ Tracker.newSession exactTopic def
    eses <- liftIO $ Exchange.newSession clientLogger (externalAddr c)
                     rootPath (error "openMagnet" exactTopic)
    return $ Handle
      { handleTopic    = exactTopic
      , handlePrivate  = False
      , handleTrackers = tses
      , handleExchange = eses
      }

-- | Stop torrent and destroy all sessions. You don't need to close
-- handles at application exit, all handles will be automatically
-- closed at 'Network.BitTorrent.Client.closeClient'. This operation
-- may block.
closeHandle :: Handle -> BitTorrent ()
closeHandle h @ Handle {..} = do
  freeHandle handleTopic $ do
    stop h
    liftIO $ Exchange.closeSession handleExchange
    liftIO $ Tracker.closeSession handleTrackers

{-----------------------------------------------------------------------
--  Control
-----------------------------------------------------------------------}

-- | Start downloading, uploading and announcing this torrent.
--
-- This operation is blocking, use
-- 'Control.Concurrent.Async.Lifted.async' if needed.
start :: Handle -> BitTorrent ()
start Handle {..} = do
  Client {..} <- getClient
  liftIO $ Tracker.notify trackerManager handleTrackers Tracker.Started
  unless handlePrivate $ do
    liftDHT $ DHT.insert handleTopic (error "start")
  liftIO $ do
    peers <- askPeers trackerManager handleTrackers
    print $ "got: " ++ show (L.length peers) ++ " peers"
    forM_ peers $ \ peer -> do
      Exchange.connect peer handleExchange

-- | Stop downloading this torrent.
pause :: Handle -> BitTorrent ()
pause _ = return ()

-- | Stop downloading, uploading and announcing this torrent.
stop :: Handle -> BitTorrent ()
stop Handle {..} = do
  Client {..} <- getClient
  unless handlePrivate $ do
    liftDHT $ DHT.delete handleTopic (error "stop")
  liftIO  $ Tracker.notify trackerManager handleTrackers Tracker.Stopped

{-----------------------------------------------------------------------
--  Query
-----------------------------------------------------------------------}

data HandleState
  = Running
  | Paused
  | Stopped

getHandle :: InfoHash -> BitTorrent Handle
getHandle ih = do
  mhandle <- lookupHandle ih
  case mhandle of
    Nothing -> error "should we throw some exception?"
    Just h  -> return h

getState :: Handle -> IO HandleState
getState = undefined