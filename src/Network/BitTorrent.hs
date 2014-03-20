-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent
       ( -- * Client
         Options (..)

         -- ** Session
       , Client
       , clientPeerId
       , clientListenerPort
       , allowedExtensions

         -- ** Initialization
       , LogFun
       , newClient
       , closeClient
       , withClient

         -- ** Monadic
       , MonadBitTorrent (..)
       , BitTorrent
       , runBitTorrent
       , getClient
       , simpleClient

         -- * Torrent
         -- ** Source
       , InfoHash
       , Magnet
       , InfoDict
       , Torrent

         -- ** Handle
       , Handle
       , handleTopic
       , handleTrackers

       , TorrentSource(openHandle)
       , closeHandle
       , getHandle
       , getIndex

         -- ** Control
       , start
       , pause
       , stop
       ) where

import Data.Torrent as BT
import Data.Torrent.InfoHash as BT
import Data.Torrent.Magnet as BT
import Network.BitTorrent.Client as BT