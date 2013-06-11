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
       , P2P, PeerSession
            ( connectedPeerAddr, enabledExtensions
            , peerBitfield, peerSessionStatus
            )

       , awaitEvent, signalEvent
       ) where

import Control.Monad
import Data.IORef

import Data.Torrent
import Network.BitTorrent.Internal
import Network.BitTorrent.Exchange
import Network.BitTorrent.Tracker



-- discover should hide tracker and DHT communication under the hood
-- thus we can obtain unified interface

discover :: SwarmSession -> P2P () -> IO ()
discover swarm @ SwarmSession {..} action = do
  let conn = TConnection (tAnnounce torrentMeta) (tInfoHash torrentMeta)
                         (clientPeerID clientSession) port
  progress <- readIORef (currentProgress clientSession)
  withTracker progress conn $ \tses -> do
    forever $ do
      addr <- getPeerAddr tses
      withPeer swarm addr action


port = 10000
