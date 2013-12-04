-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Re-export every @Network.BitTorrent.Core.*@ module.
--
module Network.BitTorrent.Core (module Core) where
import Network.BitTorrent.Core.PeerId   as Core
import Network.BitTorrent.Core.PeerAddr as Core