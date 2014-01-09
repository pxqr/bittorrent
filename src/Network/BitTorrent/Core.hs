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

         -- * Address class
       , Address (..)
       , fromAddr

         -- * Re-exports from Data.IP
       , IPv4
       , IPv6
       , IP (..)
       ) where

import Control.Applicative
import Data.IP
import Data.Hashable
import Data.Serialize
import Data.Time
import Data.Typeable
import Network.Socket (SockAddr (..), PortNumber)
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class

import Network.BitTorrent.Core.Fingerprint as Core
import Network.BitTorrent.Core.NodeInfo    as Core
import Network.BitTorrent.Core.PeerId      as Core
import Network.BitTorrent.Core.PeerAddr    as Core


instance Pretty UTCTime where
  pretty = PP.text . show

class (Eq a, Serialize a, Typeable a, Hashable a, Pretty a)
    => Address a where
  toSockAddr   :: a        -> SockAddr
  fromSockAddr :: SockAddr -> Maybe a

fromAddr :: (Address a, Address b) => a -> Maybe b
fromAddr = fromSockAddr . toSockAddr

-- | Note that port is zeroed.
instance Address IPv4 where
  toSockAddr = SockAddrInet 0 . toHostAddress
  fromSockAddr (SockAddrInet _ h) = Just (fromHostAddress h)
  fromSockAddr  _                 = Nothing

-- | Note that port is zeroed.
instance Address IPv6 where
  toSockAddr h = SockAddrInet6 0 0 (toHostAddress6 h) 0
  fromSockAddr (SockAddrInet6 _ _ h _) = Just (fromHostAddress6 h)
  fromSockAddr  _                      = Nothing

-- | Note that port is zeroed.
instance Address IP where
  toSockAddr (IPv4 h) = toSockAddr h
  toSockAddr (IPv6 h) = toSockAddr h
  fromSockAddr sa =
        IPv4 <$> fromSockAddr sa
    <|> IPv6 <$> fromSockAddr sa

setPort :: PortNumber -> SockAddr -> SockAddr
setPort port (SockAddrInet  _   h  ) = SockAddrInet  port   h
setPort port (SockAddrInet6 _ f h s) = SockAddrInet6 port f h s
setPort _    (SockAddrUnix  s      ) = SockAddrUnix  s
{-# INLINE setPort #-}

getPort :: SockAddr -> Maybe PortNumber
getPort (SockAddrInet  p   _  ) = Just p
getPort (SockAddrInet6 p _ _ _) = Just p
getPort (SockAddrUnix  _      ) = Nothing
{-# INLINE getPort #-}

instance Address a => Address (NodeAddr a) where
  toSockAddr NodeAddr {..} = setPort nodePort $ toSockAddr nodeHost
  fromSockAddr sa = NodeAddr <$> fromSockAddr sa <*> getPort sa

instance Address a => Address (PeerAddr a) where
  toSockAddr PeerAddr {..} = setPort peerPort $ toSockAddr peerHost
  fromSockAddr sa = PeerAddr Nothing <$> fromSockAddr sa <*> getPort sa
