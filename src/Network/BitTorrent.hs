-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent
       ( module Data.Torrent

       , TorrentLoc(..), Progress(..)
       , ThreadCount, SessionCount

       , ClientSession( clientPeerId, allowedExtensions )
       , withDefaultClient, defaultThreadCount
       , addTorrent
       , removeTorrent

       , getCurrentProgress
       , getPeerCount
       , getSwarmCount
       , getSessionCount

         -- * Extensions
       , Extension
       , defaultExtensions
       , ppExtension
       ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Network
import Data.Torrent
import Network.BitTorrent.Sessions.Types
import Network.BitTorrent.Sessions
import Network.BitTorrent.Extension
import Network.BitTorrent.Exchange

import System.Torrent.Storage

-- TODO remove fork from Network.BitTorrent.Exchange
-- TODO make all forks in Internal.

-- | Client session with default parameters. Use it for testing only.
withDefaultClient :: PortNumber -> PortNumber -> (ClientSession -> IO ()) -> IO ()
withDefaultClient dhtPort listPort action = do
  withClientSession defaultThreadCount defaultExtensions listPort dhtPort action

{-----------------------------------------------------------------------
    Torrent management
-----------------------------------------------------------------------}

-- | Used to check torrent location before register torrent.
validateLocation :: TorrentLoc -> IO Torrent
validateLocation TorrentLoc {..} = do
  t <- fromFile metafilePath
--  exists <- doesDirectoryExist dataDirPath
--  unless exists $ do
--    throw undefined
  return t


-- | Register torrent and start downloading.
addTorrent :: ClientSession -> TorrentLoc -> IO ()
addTorrent clientSession loc @ TorrentLoc {..} = do
  torrent <- validateLocation loc
--  registerTorrent loc tInfoHash
--  when (bf is not full)

  swarm   <- newLeecher  clientSession torrent
  storage <- openStorage (torrentMeta swarm) dataDirPath
  forkIO $ discover swarm $ do
    liftIO $ putStrLn "connected to peer"
    forever $ do
      liftIO $ putStrLn "from mesage loop"
      exchange storage
  return ()

-- | Unregister torrent and stop all running sessions.
removeTorrent :: ClientSession -> InfoHash ->  IO ()
removeTorrent ses loc = undefined -- atomically $ unregisterTorrent ses loc

{-
-- | The same as 'removeTorrrent' torrent, but delete all torrent
--   content files.
deleteTorrent :: ClientSession -> TorrentLoc -> IO ()
deleteTorrent ClientSession {..} TorrentLoc {..} = undefined
-}