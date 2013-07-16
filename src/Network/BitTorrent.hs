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

       , TorrentLoc(..), TorrentMap, Progress(..)
       , ThreadCount, SessionCount

       , ClientSession( clientPeerId, allowedExtensions )
       , withDefaultClient, defaultThreadCount, defaultPorts
       , addTorrent
       , removeTorrent

       , getCurrentProgress
       , getPeerCount
       , getSwarmCount
       , getSessionCount
       , getSwarm
       , getStorage
       , getTorrentInfo

         -- * Extensions
       , Extension
       , defaultExtensions
       , ppExtension
       ) where

import Network
import Data.Torrent
import Network.BitTorrent.Sessions.Types
import Network.BitTorrent.Sessions
import Network.BitTorrent.Extension
import Network.BitTorrent.Tracker

-- TODO remove fork from Network.BitTorrent.Exchange
-- TODO make all forks in Internal.

-- | Client session with default parameters. Use it for testing only.
withDefaultClient :: PortNumber -> PortNumber -> (ClientSession -> IO ()) -> IO ()
withDefaultClient listPort dhtPort action = do
  withClientSession defaultThreadCount [] listPort dhtPort action

{-----------------------------------------------------------------------
    Torrent management
-----------------------------------------------------------------------}

-- | Register torrent and start downloading.
addTorrent :: ClientSession -> TorrentLoc -> IO ()
addTorrent cs loc @ TorrentLoc {..} = do
  registerTorrent  cs loc
  openSwarmSession cs loc
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
