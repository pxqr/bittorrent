-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
module Network.BitTorrent.Exchange
       ( -- * Options
         Options (..)
       , Caps
       , Extension
       , toCaps

         -- * Manager
       , Manager
       , Handler
       , newManager
       , closeManager

         -- * Session
       , Session
       , newSession
       , closeSession

         -- * Session control
       , insert
       ) where

import Network.BitTorrent.Exchange.Manager
import Network.BitTorrent.Exchange.Message
import Network.BitTorrent.Exchange.Session