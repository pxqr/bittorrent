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
import Control.Exception
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.TH
import Data.BEncode   as BS
import Data.BEncode.BDict (BKey)
import Data.ByteString
import Data.ByteString.Char8 as BS8
import Data.Bits
import Data.Char
import Data.Default
import Data.List      as L
import Data.List.Split
import Data.Serialize as S
import Data.String
import Data.Typeable
import Data.Word
import Data.IP
import Network.Socket
import Text.PrettyPrint
import Text.PrettyPrint.Class
import Text.Read (readMaybe)
import System.IO.Unsafe

import Data.Torrent.JSON
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
data PeerAddr = PeerAddr
  { peerId   :: !(Maybe PeerId)
  , peerIP   :: {-# UNPACK #-} !IP
  , peerPort :: {-# UNPACK #-} !PortNumber
  } deriving (Show, Eq, Typeable)

instance BEncode IP where
    toBEncode ip = toBEncode $ BS8.pack $ show ip
    fromBEncode (BString ip) = return $ fromString $ BS8.unpack ip

peer_id_key, peer_ip_key, peer_port_key :: BKey
peer_id_key   = "peer id"
peer_ip_key   = "ip"
peer_port_key = "port"

-- FIXME do we need to byteswap peerIP in bencode instance?
-- | The tracker's 'announce response' compatible encoding.
instance BEncode PeerAddr where
  toBEncode PeerAddr {..} = toDict $
       peer_id_key   .=? peerId
    .: peer_ip_key   .=! BS8.pack (show peerIP)
    .: peer_port_key .=! peerPort
    .: endDict

  fromBEncode = fromDict $ do
    PeerAddr <$>? peer_id_key
             <*>! peer_ip_key
             <*>! peer_port_key

-- | The tracker's 'compact peer list' compatible encoding. The
-- 'peerId' is always 'Nothing'.
--
--   For more info see: <http://www.bittorrent.org/beps/bep_0023.html>
--
instance Serialize PeerAddr where -- TODO do it properly
  put PeerAddr {..} = (putWord32host $ toHostAddress $ ipv4 peerIP) >> put peerPort
  {-# INLINE put #-}
  get = PeerAddr Nothing <$> (IPv4 . fromHostAddress <$> getWord32host) <*> get
  {-# INLINE get #-}

-- | @127.0.0.1:6881@
instance Default PeerAddr where
  def = "127.0.0.1:6881"

-- inet_addr is pure; so it is safe to throw IO
unsafeCatchIO :: IO a -> Maybe a
unsafeCatchIO m = unsafePerformIO $
    catch (m >>= evaluate >>= return . Just) handler
  where
    handler :: IOError -> IO (Maybe a)
    handler _ = pure Nothing

-- | Example:
--
--   @peerPort \"127.0.0.1:6881\" == 6881@
--
instance IsString PeerAddr where
  fromString str -- TODO IPv6
    | [hostAddrStr, portStr] <- splitWhen (== ':') str
    , Just hostAddr <- read hostAddrStr
    , Just portNum  <- toEnum <$> readMaybe portStr
                = PeerAddr Nothing hostAddr portNum
    | otherwise = error $ "fromString: unable to parse PeerAddr: " ++ str

-- | fingerprint + "at" + dotted.host.inet.addr:port
instance Pretty PeerAddr where
  pretty p @ PeerAddr {..}
    | Just pid <- peerId = pretty (fingerprint pid) <+> "at" <+> paddr
    |     otherwise      = paddr
    where
      paddr = text (show (peerSockAddr p))

-- | Ports typically reserved for bittorrent P2P listener.
defaultPorts :: [PortNumber]
defaultPorts =  [6881..6889]

-- | Convert peer info from tracker response to socket address.  Used
--   for establish connection between peers.
--
peerSockAddr :: PeerAddr -> SockAddr
peerSockAddr PeerAddr {..}
    | IPv4 v4 <- peerIP = SockAddrInet peerPort (toHostAddress v4)
    | IPv6 v6 <- peerIP = SockAddrInet6 peerPort 0 (toHostAddress6 v6) 0
