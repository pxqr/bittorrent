-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   'PeerAddr' is used to represent peer location. Currently it's
--   just peer IP and peer port but this might be changed later.
--
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -fno-warn-orphans           #-} -- for PortNumber instances
module Network.BitTorrent.Core.PeerAddr
       ( -- * Peer address
         PeerAddr(..)
       , getCompactPeerList
       , peerSockAddr
       , connectToPeer
       , ppPeer
       ) where

import Control.Applicative
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.TH
import Data.BEncode   as BS
import Data.Bits
import Data.Char
import Data.List      as L
import Data.Serialize as S
import Data.Typeable
import Data.Word
import Network.Socket
import Text.PrettyPrint

import Data.Torrent.Client
import Network.BitTorrent.Core.PeerId


deriving instance ToJSON PortNumber
deriving instance FromJSON PortNumber

instance BEncode PortNumber where
  toBEncode = toBEncode . fromEnum
  fromBEncode b = toEnum <$> fromBEncode b

instance Serialize PortNumber where
  get = fromIntegral <$> getWord16be
  {-# INLINE get #-}
  put = putWord16be . fromIntegral
  {-# INLINE put #-}

-- TODO check semantic of ord and eq instances

-- | Peer address info normally extracted from peer list or peer
-- compact list encoding.
data PeerAddr = PeerAddr {
      peerID   :: Maybe PeerId
    , peerIP   :: {-# UNPACK #-} !HostAddress
    , peerPort :: {-# UNPACK #-} !PortNumber
    } deriving (Show, Eq, Ord, Typeable)

$(deriveJSON (L.map toLower . L.dropWhile isLower) ''PeerAddr)

instance BEncode PeerAddr where
  toBEncode (PeerAddr pid pip pport) = toDict $
       "peer id" .=? pid
    .: "ip"      .=!  pip
    .: "port"    .=!  pport
    .: endDict

  fromBEncode = fromDict $ do
    PeerAddr <$>? "peer id"
             <*>! "ip"
             <*>! "port"

instance Serialize PeerAddr where
  put PeerAddr {..} = put peerID >> put peerPort
  {-# INLINE put #-}
  get = PeerAddr Nothing <$> get <*> get
  {-# INLINE get #-}

getCompactPeerList :: S.Get [PeerAddr]
getCompactPeerList = many get

-- TODO make platform independent, clarify htonl

-- | Convert peer info from tracker response to socket address.  Used
--   for establish connection between peers.
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

-- | Pretty print peer address in human readable form.
ppPeer :: PeerAddr -> Doc
ppPeer p @ PeerAddr {..} = case peerID of
    Just pid -> ppClientInfo (clientInfo pid) <+> "at" <+> paddr
    Nothing  -> paddr
  where
    paddr = text (show (peerSockAddr p))
