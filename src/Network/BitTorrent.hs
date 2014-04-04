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
       , handleExchange

       , TorrentSource(openHandle)
       , closeHandle
       , getHandle
       , getIndex

         -- ** Control
       , start
       , pause
       , stop

         -- * Events
       , EventSource (..)
       ) where

import Data.Torrent
import Data.Torrent.InfoHash
import Data.Torrent.Magnet
import Network.BitTorrent.Client
import Network.BitTorrent.Internal.Types