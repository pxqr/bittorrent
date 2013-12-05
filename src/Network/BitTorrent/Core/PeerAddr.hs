-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   'PeerAddr' is used to represent peer address. Currently it's
--   just peer IP and peer port but this might change in future.
--
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -fno-warn-orphans           #-} -- for PortNumber instances
module Network.BitTorrent.Core.PeerAddr
       ( -- * Peer address
         PeerAddr(..)
       , defaultPorts
       , peerSockAddr
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
import Text.PrettyPrint.Class

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
-- TODO use SockAddr instead of peerIP and peerPort

-- | Peer address info normally extracted from peer list or peer
-- compact list encoding.
data PeerAddr = PeerAddr {
      peerID   :: !(Maybe PeerId)
    , peerIP   :: {-# UNPACK #-} !HostAddress
    , peerPort :: {-# UNPACK #-} !PortNumber
    } deriving (Show, Eq, Ord, Typeable)

$(deriveJSON defaultOptions { fieldLabelModifier = (L.map toLower . L.dropWhile isLower) } ''PeerAddr)

-- | The tracker "announce query" compatible encoding.
instance BEncode PeerAddr where
  toBEncode (PeerAddr pid pip pport) = toDict $
       "peer id" .=? pid
    .: "ip"      .=! pip
    .: "port"    .=! pport
    .: endDict

  fromBEncode = fromDict $ do
    PeerAddr <$>? "peer id"
             <*>! "ip"
             <*>! "port"

-- | The tracker "compact peer list" compatible encoding. The
-- 'peerId' is always 'Nothing'.
--
--   For more info see: <http://www.bittorrent.org/beps/bep_0023.html>
--
instance Serialize PeerAddr where
  put PeerAddr {..} = put peerID >> put peerPort
  {-# INLINE put #-}
  get = PeerAddr Nothing <$> get <*> get
  {-# INLINE get #-}

instance Pretty PeerAddr where
  pretty p @ PeerAddr {..}
    | Just pid <- peerID = pretty (fingerprint pid) <+> "at" <+> paddr
    |     otherwise      = paddr
    where
      paddr = text (show (peerSockAddr p))

-- | Ports typically reserved for bittorrent P2P listener.
defaultPorts :: [PortNumber]
defaultPorts =  [6881..6889]

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
