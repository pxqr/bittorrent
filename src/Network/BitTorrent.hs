-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
module Network.BitTorrent
       ( module BT

         -- * Tracker

         -- * P2P
       , ClientSession, newClient
       , SwarmSession, newLeacher, newSeeder
       , PeerSession
       ) where

import Network.BitTorrent.Internal

import Network.BitTorrent.Extension as BT
import Network.BitTorrent.Peer as BT
import Network.BitTorrent.Exchange as BT
import Network.BitTorrent.Tracker as BT

--discover :: SwarmSession -> (Chan PeerAddr -> IO a) -> IO a
--discover = undefined
