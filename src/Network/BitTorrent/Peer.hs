-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
module Network.BitTorrent.Peer
       ( Peer(..)
       , peerSockAddr, connectToPeer
       , ppPeer
       ) where

import Control.Applicative
import Data.Word
import Data.Bits
import Network
import Network.Socket

import Network.BitTorrent.PeerID
import Network.BitTorrent.PeerWire.ClientInfo


data Peer = Peer {
      peerID   :: Maybe PeerID
    , peerIP   :: HostAddress
    , peerPort :: PortNumber
    } deriving Show

-- TODO make platform independent, clarify htonl
-- | Convert peer info from tracker response to socket address.
--   Used for establish connection between peers.
--
peerSockAddr :: Peer -> SockAddr
peerSockAddr = SockAddrInet <$> (g . peerPort) <*> (htonl . peerIP)
  where
    htonl :: Word32 -> Word32
    htonl d =
       ((d .&. 0xff) `shiftL` 24) .|.
       (((d `shiftR` 8 ) .&. 0xff) `shiftL` 16) .|.
       (((d `shiftR` 16) .&. 0xff) `shiftL` 8)  .|.
       ((d `shiftR` 24) .&. 0xff)

    g :: PortNumber -> PortNumber
    g = id

-- | Tries to connect to peer using reasonable default parameters.
connectToPeer :: Peer -> IO Socket
connectToPeer p = do
  sock <- socket AF_INET Stream Network.Socket.defaultProtocol
  connect sock (peerSockAddr p)
  return sock

ppPeer :: Peer -> String
ppPeer p = maybe ""  (++ " at ") ((ppClientInfo . clientInfo) <$> peerID p)
        ++ show (peerSockAddr p)
