-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent
       ( module BT
       ) where

import Data.Torrent as BT
import Data.Torrent.InfoHash as BT
import Data.Torrent.Magnet as BT
import Network.BitTorrent.Client as BT