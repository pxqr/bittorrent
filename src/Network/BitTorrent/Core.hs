-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Re-export every @Network.BitTorrent.Core.*@ module.
--
module Network.BitTorrent.Core
       ( module Core

         -- * Re-exports from Data.IP
       , IPv4
       , IPv6
       , IP (..)
       ) where

import Data.IP

import Network.BitTorrent.Core.Fingerprint as Core
import Network.BitTorrent.Core.PeerId      as Core
import Network.BitTorrent.Core.PeerAddr    as Core
