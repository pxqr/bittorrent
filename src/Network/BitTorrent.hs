-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent
       ( module BT
       , module Data.Torrent

         -- * Tracker

         -- * P2P
       , ClientSession, newClient
       , SwarmSession, newLeacher, newSeeder
       , PeerSession
       , discover
       ) where

import Data.IORef

import Data.Torrent
import Network.BitTorrent.Internal
import Network.BitTorrent.Extension as BT
import Network.BitTorrent.Peer as BT
import Network.BitTorrent.Exchange as BT
import Network.BitTorrent.Tracker as BT

-- discover should hide tracker and DHT communication under the hood
-- thus we can obtain unified interface

discover :: SwarmSession -> (TSession -> IO a) -> IO a
discover SwarmSession {..} action = do
  let conn = TConnection (tAnnounce torrentMeta) (tInfoHash torrentMeta)
                         (clientPeerID clientSession) port
  progress <- readIORef (currentProgress clientSession)
  withTracker progress conn action

port = 10000
