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
       , ClientSession
       , newClient

       , SwarmSession
       , newLeacher, newSeeder

         -- * Discovery
       , discover

         -- * Peer to Peer
       , P2P
       , Event(..)
       , PeerSession ( connectedPeerAddr, enabledExtensions )
       , Block(..), BlockIx(..), ppBlock, ppBlockIx

       , awaitEvent, yieldEvent
       ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader

import Network

import Data.Torrent
import Network.BitTorrent.Internal
import Network.BitTorrent.Exchange
import Network.BitTorrent.Exchange.Protocol
import Network.BitTorrent.Tracker


-- discover should hide tracker and DHT communication under the hood
-- thus we can obtain an unified interface

discover :: SwarmSession -> P2P () -> IO ()
discover swarm action = do
  port <- listener swarm action

  let conn = TConnection (tAnnounce (torrentMeta swarm))
                         (tInfoHash (torrentMeta swarm))
                         (clientPeerID (clientSession swarm))
                          port

  progress <- getCurrentProgress (clientSession swarm)

  putStrLn "lookup peers"
  withTracker progress conn $ \tses -> do
    putStrLn "get peer list "
    forever $ do
      addr <- getPeerAddr tses
      putStrLn "connect to peer"
      spawnP2P swarm addr $ do
        liftIO $ putStrLn "run p2p session"
        action
      putStrLn "connected"

listener :: SwarmSession -> P2P () -> IO PortNumber
listener _ _ = do
  -- TODO:
--  forkIO loop
  return 10000
