-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
module Network.BitTorrent.Exchange
       ( -- * Manager
         Options (..)
       , Manager
       , Handler
       , newManager
       , closeManager

         -- * Session
       , Caps
       , Extension
       , toCaps
       , Session
       , newSession
       , closeSession

         -- * Query
       , waitMetadata
       , takeMetadata

         -- * Connections
       , connect

         -- * Events
       , SessionEvent    (..)
       , subscribe
       ) where

import Network.BitTorrent.Exchange.Connection hiding (Options)
import Network.BitTorrent.Exchange.Manager
import Network.BitTorrent.Exchange.Message
import Network.BitTorrent.Exchange.Session
