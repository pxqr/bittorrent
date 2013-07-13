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
      forkThrottle swarm $ do
        initiatePeerSession swarm addr $ \conn ->
          runP2P conn action


startListener :: ClientSession -> PortNumber -> IO ()
startListener cs @ ClientSession {..} port =
  startService peerListener port $ listener cs $ \conn @ (sock, PeerSession{..}) -> do
      print "accepted"
      let storage = error "storage"
      runP2P conn (exchange storage)

startDHT :: ClientSession -> PortNumber -> IO ()
startDHT ClientSession {..} nodePort = withRunning peerListener failure start
  where
    start ClientService {..} = do
      ses  <- newNodeSession servPort
      startService nodeListener nodePort (dhtServer ses)

    failure = throwIO $ userError msg
    msg = "unable to start DHT server: peer listener is not running"
