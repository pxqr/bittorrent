-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
--   This module provides high level API for peer -> tracker
--   communication. Tracker is used to discover other peers in the
--   network using torrent info hash.
--
{-# LANGUAGE TemplateHaskell   #-}
module Network.BitTorrent.Tracker
       ( PeerInfo (..)

         -- * RPC Manager
       , Options
       , Manager
       , newManager
       , closeManager
       , withManager

         -- * Multitracker session
       , trackerList
       , Session
       , newSession
       , closeSession

         -- * Events
       , Event (..)
       , notify
       , askPeers

         -- * Query
--       , getSessionState
       ) where

import Network.BitTorrent.Tracker.List
import Network.BitTorrent.Tracker.RPC
import Network.BitTorrent.Tracker.Session
