-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS  -fno-warn-orphans #-}
module Network.BitTorrent.Peer.Addr
       ( PeerAddr(..)
       , peerSockAddr, connectToPeer
       , ppPeer
       ) where

import Control.Applicative
import Data.BEncode
import Data.Bits
import Data.Word
import Text.PrettyPrint
import Network
import Network.Socket

import Network.BitTorrent.Peer.ID
import Network.BitTorrent.Peer.ClientInfo


data PeerAddr = PeerAddr {
      peerID   :: Maybe PeerID
    , peerIP   :: HostAddress
    , peerPort :: PortNumber
    } deriving (Show, Eq)

instance BEncodable PortNumber where
  toBEncode = toBEncode . fromEnum
  fromBEncode b = toEnum <$> fromBEncode b

instance BEncodable PeerAddr where
  toBEncode (PeerAddr pid pip pport) = fromAssocs
    [ "peer id" -->? pid
    , "ip"      -->  pip
    , "port"    -->  pport
    ]

  fromBEncode (BDict d) =
    PeerAddr <$> d >--? "peer id"
             <*> d >--  "ip"
             <*> d >--  "port"

  fromBEncode _ = decodingError "PeerAddr"


-- TODO make platform independent, clarify htonl
-- | Convert peer info from tracker response to socket address.
--   Used for establish connection between peers.
--
peerSockAddr :: PeerAddr -> SockAddr
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
connectToPeer :: PeerAddr -> IO Socket
connectToPeer p = do
  sock <- socket AF_INET Stream Network.Socket.defaultProtocol
  connect sock (peerSockAddr p)
  return sock

ppPeer :: PeerAddr -> Doc
ppPeer p @ PeerAddr {..} = case peerID of
    Just pid -> ppClientInfo (clientInfo pid) <+> "at" <+> paddr
    Nothing  -> paddr
  where
    paddr = text (show (peerSockAddr p))
