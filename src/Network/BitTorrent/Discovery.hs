{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.Discovery
       (discover, startListener, startDHT
       ) where

import Control.Monad
import Control.Concurrent
import Control.Exception
import Network.Socket

import Data.Torrent
import Network.BitTorrent.Peer
import Network.BitTorrent.Internal
import Network.BitTorrent.Exchange
import Network.BitTorrent.Tracker
import Network.BitTorrent.DHT


-- discover should hide tracker and DHT communication under the hood
-- thus we can obtain an unified interface

discover :: SwarmSession -> P2P () -> IO ()
discover swarm @ SwarmSession {..} action = {-# SCC discover #-} do
  port <- listenerPort clientSession

  let conn = TConnection {
        tconnAnnounce = tAnnounce torrentMeta
      , tconnInfoHash = tInfoHash torrentMeta
      , tconnPeerId   = clientPeerId clientSession
      , tconnPort     = port
      }

  progress <- getCurrentProgress clientSession

  withTracker progress conn $ \tses -> do
    forever $ do
      addr <- getPeerAddr tses
      spawnP2P swarm addr $ do
        action

startListener :: ClientSession -> PortNumber -> IO ()
startListener cs @ ClientSession {..} port =
  putMVar peerListener =<< startService port (listener cs (error "listener"))

startDHT :: ClientSession -> PortNumber -> IO ()
startDHT ClientSession {..} nodePort = do
    maybe failure start =<< tryTakeMVar peerListener
  where
    start ClientService {..} = do
      ses  <- newNodeSession servPort
      serv <- startService nodePort (dhtServer ses)
      putMVar nodeListener serv

    failure = throwIO $ userError msg
    msg = "unable to start DHT server: peer listener is not running"
