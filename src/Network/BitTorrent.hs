-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent
       (
         module Data.Torrent

         -- * Session
       , ThreadCount
       , defaultThreadCount

         -- ** Client
       , ClientSession( clientPeerId, allowedExtensions )

       , withDefaultClient

       , Progress(..)
       , getCurrentProgress
       , getPeerCount
       , getSwarmCount

       , TorrentLoc(..)
       , addTorrent
       , removeTorrent

         -- ** Swarm
       , SwarmSession(torrentMeta)

       , newLeecher
       , newSeeder

       , SessionCount
       , getSessionCount

         -- * Discovery
       , discover
       , exchange


         -- * Peer to Peer
       , P2P

         -- ** Session
       , PeerSession( PeerSession, connectedPeerAddr
                    , swarmSession, enabledExtensions
                    )

       , getHaveCount
       , getWantCount
       , getPieceCount


         -- ** Transfer
       , Block(..), ppBlock
       , BlockIx(..), ppBlockIx

         -- ** Control
       , SessionException
       , disconnect
       , protocolError

         -- ** Events
       , Event(..)
       , awaitEvent, yieldEvent

         -- * Extensions
       , Extension, defaultExtensions, ppExtension
       ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader

import Network

import Data.Bitfield as BF
import Data.Torrent
import Network.BitTorrent.Sessions.Types
import Network.BitTorrent.Sessions
import Network.BitTorrent.Peer
import Network.BitTorrent.Extension
import Network.BitTorrent.Exchange
import Network.BitTorrent.Exchange.Protocol
import Network.BitTorrent.Tracker

import System.Torrent.Storage

-- TODO remove fork from Network.BitTorrent.Exchange
-- TODO make all forks in Internal.

-- | Client session with default parameters. Use it for testing only.
withDefaultClient :: PortNumber -> PortNumber -> (ClientSession -> IO ()) -> IO ()
withDefaultClient dhtPort listPort action = do
  withClientSession defaultThreadCount defaultExtensions $ \client -> do
    action client

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