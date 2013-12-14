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
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# OPTIONS -fno-warn-orphans           #-} -- for PortNumber instances
module Network.BitTorrent.Core.PeerAddr
       ( -- * Peer address
         PeerAddr(..)
       , defaultPorts
       , peerSockAddr
       , mergeIPLists
       , splitIPList
       , IP, IPv4, IPv6 --re-export Data.IP constructors
       , IPAddress ()
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
import Data.Maybe
import Data.Foldable
import Data.Either
import Network.Socket
import Text.PrettyPrint
import Text.PrettyPrint.Class
import Text.Read (readMaybe)
import qualified Text.ParserCombinators.ReadP as RP
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

class (Show i, Read i) => IPAddress i where
    showIp :: i -> String
    showIp = show

    readIp :: String -> i
    readIp = read

    toHostAddr :: i -> Either HostAddress HostAddress6

instance IPAddress IPv4 where
    toHostAddr = Left . toHostAddress

instance IPAddress IPv6 where
    toHostAddr = Right . toHostAddress6

instance IPAddress IP where
    toHostAddr (IPv4 ip) = toHostAddr ip
    toHostAddr (IPv6 ip) = toHostAddr ip


deriving instance Typeable IP
deriving instance Typeable IPv4
deriving instance Typeable IPv6

ipToBEncode ip = BString $ BS8.pack $ showIp ip
ipFromBEncode (BString ip) = return $ readIp $ BS8.unpack ip

instance BEncode IP where
    toBEncode = ipToBEncode
    fromBEncode = ipFromBEncode

instance BEncode IPv4 where
    toBEncode = ipToBEncode
    fromBEncode = ipFromBEncode

instance BEncode IPv6 where
    toBEncode = ipToBEncode
    fromBEncode = ipFromBEncode

instance Serialize IPv4 where
    put ip = put $ toHostAddress ip
    get = fromHostAddress <$> get

instance Serialize IPv6 where
    put ip = put $ toHostAddress6 ip
    get = fromHostAddress6 <$> get

-- TODO check semantic of ord and eq instances
-- TODO use SockAddr instead of peerIP and peerPort

-- | Peer address info normally extracted from peer list or peer
-- compact list encoding.
data PeerAddr a = PeerAddr
  { peerId   :: !(Maybe PeerId)
  , peerAddr :: a
  , peerPort :: {-# UNPACK #-} !PortNumber
  } deriving (Show, Eq, Typeable, Functor)

peer_id_key, peer_ip_key, peer_port_key :: BKey
peer_id_key   = "peer id"
peer_ip_key   = "ip"
peer_port_key = "port"

-- | The tracker's 'announce response' compatible encoding.
instance (Typeable a, BEncode a) => BEncode (PeerAddr a) where
  toBEncode PeerAddr {..} = toDict $
       peer_id_key   .=? peerId
    .: peer_ip_key   .=! peerAddr
    .: peer_port_key .=! peerPort
    .: endDict

  fromBEncode = fromDict $ do
    PeerAddr <$>? peer_id_key
             <*>! peer_ip_key
             <*>! peer_port_key

mergeIPLists :: [PeerAddr IPv4] -> Maybe [PeerAddr IPv6] -> [PeerAddr IP]
mergeIPLists v4 v6 = (fmap IPv4 `L.map` v4)
                  ++ (fmap IPv6 `L.map` Data.Foldable.concat v6)

splitIPList :: [PeerAddr IP] -> ([PeerAddr IPv4],[PeerAddr IPv6])
splitIPList xs = partitionEithers $ toEither <$> xs
    where
      toEither :: PeerAddr IP -> Either (PeerAddr IPv4) (PeerAddr IPv6)
      toEither pa@(PeerAddr _ (IPv4 _) _) = Left  (ipv4 <$> pa)
      toEither pa@(PeerAddr _ (IPv6 _) _) = Right (ipv6 <$> pa)


-- | The tracker's 'compact peer list' compatible encoding. The
-- 'peerId' is always 'Nothing'.
--
--   For more info see: <http://www.bittorrent.org/beps/bep_0023.html>
--
-- TODO: test byte order
instance (Serialize a) => Serialize (PeerAddr a) where
    put PeerAddr {..} =
        put peerAddr >> put peerPort
    get =
        PeerAddr Nothing <$> get <*> get

-- | @127.0.0.1:6881@
instance Default (PeerAddr IPv4) where
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
instance IsString (PeerAddr IPv4) where
  fromString str
    | [hostAddrStr, portStr] <- splitWhen (== ':') str
    , hostAddr <- read hostAddrStr
    , Just portNum  <- toEnum <$> readMaybe portStr
                = PeerAddr Nothing hostAddr portNum
    | otherwise = error $ "fromString: unable to parse (PeerAddr IPv4): " ++ str

readsIPv6_port :: String -> [((IPv6, PortNumber), String)]
readsIPv6_port = RP.readP_to_S $ do
  ip <- RP.char '[' *> (RP.readS_to_P reads) <* RP.char ']'
  RP.char ':'
  port <- toEnum <$> read <$> (RP.many1 $ RP.satisfy isDigit) <* RP.eof
  return (ip,port)

instance IsString (PeerAddr IPv6) where
  fromString str
    | [((ip,port),"")] <- readsIPv6_port str =
        PeerAddr Nothing ip port
    | otherwise = error $ "fromString: unable to parse (PeerAddr IPv6): " ++ str

-- | fingerprint + "at" + dotted.host.inet.addr:port
-- TODO: instances for IPv6, HostName
instance Pretty (PeerAddr IP) where
  pretty p @ PeerAddr {..}
    | Just pid <- peerId = pretty (fingerprint pid) <+> "at" <+> paddr
    |     otherwise      = paddr
    where
      paddr = text (show peerAddr ++ ":" ++ show peerPort)

-- | Ports typically reserved for bittorrent P2P listener.
defaultPorts :: [PortNumber]
defaultPorts =  [6881..6889]

resolvePeerAddr :: (IPAddress i) => PeerAddr HostName -> PeerAddr i
resolvePeerAddr = undefined

-- | Convert peer info from tracker response to socket address.  Used
--   for establish connection between peers.
--
peerSockAddr :: (IPAddress i) => PeerAddr i -> SockAddr
peerSockAddr PeerAddr {..}
    | Left hAddr <- toHostAddr peerAddr =
        SockAddrInet peerPort hAddr
    | Right hAddr <- toHostAddr peerAddr =
        SockAddrInet6 peerPort 0 hAddr 0
