-- |
--   Module      :  Network.BitTorrent.Address
--   Copyright   :  (c) Sam Truzjan 2013
--                  (c) Daniel Gröber 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  provisional
--   Portability :  portable
--
--   Peer and Node addresses.
--
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -fno-warn-orphans           #-}
module Network.BitTorrent.Address
       ( -- * Address
         Address (..)
       , fromAddr

         -- ** IP
       , IPv4
       , IPv6
       , IP (..)

         -- * PeerId
         -- $peer-id
       , PeerId

         -- ** Generation
       , genPeerId
       , timestamp
       , entropy

         -- ** Encoding
       , azureusStyle
       , shadowStyle
       , defaultClientId
       , defaultVersionNumber

         -- * PeerAddr
         -- $peer-addr
       , PeerAddr(..)
       , defaultPorts
       , peerSockAddr
       , peerSocket

         -- * Node
         -- ** Id
       , NodeId
       , testIdBit
       , genNodeId
       , NodeDistance
       , distance

         -- ** Info
       , NodeAddr (..)
       , NodeInfo (..)
       , rank

         -- * Fingerprint
         -- $fingerprint
       , ClientImpl (..)
       , Fingerprint (..)
       , libFingerprint
       , fingerprint

         -- * Utils
       , libUserAgent
       ) where

import Control.Applicative
import Control.Monad
import Data.BEncode as BE
import Data.BEncode   as BS
import Data.BEncode.BDict (BKey)
import Data.Bits
import Data.ByteString as BS
import Data.ByteString.Internal as BS
import Data.ByteString.Base16 as Base16
import Data.ByteString.Char8 as BC
import Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BS
import Data.Char
import Data.Convertible
import Data.Default
import Data.Foldable
import Data.IP
import Data.List as L
import Data.List.Split as L
import Data.Maybe       (fromMaybe, catMaybes)
import Data.Monoid
import Data.Hashable
import Data.Ord
import Data.Serialize as S
import Data.String
import Data.Time
import Data.Typeable
import Data.Version
import Data.Word
import qualified Text.ParserCombinators.ReadP as RP
import Text.Read        (readMaybe)
import Network.HTTP.Types.QueryLike
import Network.Socket
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class
import System.Locale    (defaultTimeLocale)
import System.Entropy

-- import Paths_bittorrent (version)

{-----------------------------------------------------------------------
--  Address
-----------------------------------------------------------------------}

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

{-----------------------------------------------------------------------
--  Peer id
-----------------------------------------------------------------------}
-- $peer-id
--
--  'PeerID' represent self assigned peer identificator. Ideally each
--  host in the network should have unique peer id to avoid
--  collisions, therefore for peer ID generation we use good entropy
--  source. Peer ID is sent in /tracker request/, sent and received in
--  /peer handshakes/ and used in DHT queries.
--

-- TODO use unpacked Word160 form (length is known statically)

-- | Peer identifier is exactly 20 bytes long bytestring.
newtype PeerId = PeerId { getPeerId :: ByteString }
  deriving (Show, Eq, Ord, BEncode, Typeable)

peerIdLen :: Int
peerIdLen = 20

-- | For testing purposes only.
instance Default PeerId where
  def = azureusStyle defaultClientId defaultVersionNumber ""

instance Hashable PeerId where
  hashWithSalt = hashUsing getPeerId
  {-# INLINE hashWithSalt #-}

instance Serialize PeerId where
  put = putByteString . getPeerId
  get = PeerId <$> getBytes peerIdLen

instance QueryValueLike PeerId where
  toQueryValue (PeerId pid) = Just pid
  {-# INLINE toQueryValue #-}

instance IsString PeerId where
  fromString str
      | BS.length bs == peerIdLen = PeerId bs
      | otherwise = error $ "Peer id should be 20 bytes long: " ++ show str
    where
      bs = fromString str

instance Pretty PeerId where
  pretty = text . BC.unpack . getPeerId

instance Convertible BS.ByteString PeerId where
  safeConvert bs
    | BS.length bs == peerIdLen = pure (PeerId bs)
    |          otherwise        = convError "invalid length" bs

------------------------------------------------------------------------

-- | Pad bytestring so it's becomes exactly request length. Conversion
-- is done like so:
--
--     * length < size: Complete bytestring by given charaters.
--
--     * length = size: Output bytestring as is.
--
--     * length > size: Drop last (length - size) charaters from a
--     given bytestring.
--
byteStringPadded :: ByteString -- ^ bytestring to be padded.
                 -> Int        -- ^ size of result builder.
                 -> Char       -- ^ character used for padding.
                 -> BS.Builder
byteStringPadded bs s c =
      BS.byteString (BS.take s bs) <>
      BS.byteString (BC.replicate padLen c)
  where
    padLen = s - min (BS.length bs) s

-- | Azureus-style encoding have the following layout:
--
--     * 1  byte : '-'
--
--     * 2  bytes: client id
--
--     * 4  bytes: version number
--
--     * 1  byte : '-'
--
--     * 12 bytes: random number
--
azureusStyle :: ByteString -- ^ 2 character client ID, padded with 'H'.
             -> ByteString -- ^ Version number, padded with 'X'.
             -> ByteString -- ^ Random number, padded with '0'.
             -> PeerId     -- ^ Azureus-style encoded peer ID.
azureusStyle cid ver rnd = PeerId $ BL.toStrict $ BS.toLazyByteString $
    BS.char8 '-' <>
      byteStringPadded cid 2  'H' <>
      byteStringPadded ver 4  'X' <>
    BS.char8 '-' <>
      byteStringPadded rnd 12 '0'

-- | Shadow-style encoding have the following layout:
--
--     * 1 byte   : client id.
--
--     * 0-4 bytes: version number. If less than 4 then padded with
--     '-' char.
--
--     * 15 bytes : random number. If length is less than 15 then
--     padded with '0' char.
--
shadowStyle :: Char       -- ^ Client ID.
            -> ByteString -- ^ Version number.
            -> ByteString -- ^ Random number.
            -> PeerId     -- ^ Shadow style encoded peer ID.
shadowStyle cid ver rnd = PeerId $ BL.toStrict $ BS.toLazyByteString $
    BS.char8 cid <>
      byteStringPadded ver 4  '-' <>
      byteStringPadded rnd 15 '0'


-- | 'HS'- 2 bytes long client identifier.
defaultClientId :: ByteString
defaultClientId = "HS"

-- | Gives exactly 4 bytes long version number for any version of the
-- package.  Version is taken from .cabal file.
defaultVersionNumber :: ByteString
defaultVersionNumber = BS.take 4 $ BC.pack $ foldMap show $
                         versionBranch $ ciVersion libFingerprint

------------------------------------------------------------------------

-- | Gives 15 characters long decimal timestamp such that:
--
--     * 6 bytes   : first 6 characters from picoseconds obtained with %q.
--
--     * 1 byte    : character \'.\' for readability.
--
--     * 9..* bytes: number of whole seconds since the Unix epoch
--     (!)REVERSED.
--
--   Can be used both with shadow and azureus style encoding. This
--   format is used to make the ID's readable for debugging purposes.
--
timestamp :: IO ByteString
timestamp = (BC.pack . format) <$> getCurrentTime
  where
    format t = L.take 6 (formatTime defaultTimeLocale "%q" t) ++ "." ++
               L.take 9 (L.reverse (formatTime defaultTimeLocale "%s" t))

-- | Gives 15 character long random bytestring. This is more robust
-- method for generation of random part of peer ID than 'timestamp'.
entropy :: IO ByteString
entropy = getEntropy 15

-- NOTE: entropy generates incorrrect peer id

-- |  Here we use 'azureusStyle' encoding with the following args:
--
--      * 'HS' for the client id; ('defaultClientId')
--
--      * Version of the package for the version number;
--      ('defaultVersionNumber')
--
--      * UTC time day ++ day time for the random number. ('timestamp')
--
genPeerId :: IO PeerId
genPeerId = azureusStyle defaultClientId defaultVersionNumber <$> timestamp

{-----------------------------------------------------------------------
-- Peer Addr
-----------------------------------------------------------------------}
--   $peer-addr
--
--   'PeerAddr' is used to represent peer address. Currently it's
--   just peer IP and peer port but this might change in future.
--

{-----------------------------------------------------------------------
--  Port number
-----------------------------------------------------------------------}

instance BEncode PortNumber where
  toBEncode   = toBEncode    .  fromEnum
  fromBEncode = fromBEncode >=> portNumber
    where
      portNumber :: Integer -> BS.Result PortNumber
      portNumber n
        | 0 <= n && n <= fromIntegral (maxBound :: Word16)
        = pure $ fromIntegral n
        | otherwise = decodingError $ "PortNumber: " ++ show n

instance Serialize PortNumber where
  get = fromIntegral <$> getWord16be
  {-# INLINE get #-}
  put = putWord16be . fromIntegral
  {-# INLINE put #-}

instance Hashable PortNumber where
  hashWithSalt s = hashWithSalt s . fromEnum
  {-# INLINE hashWithSalt #-}

instance Pretty PortNumber where
  pretty = PP.int . fromEnum
  {-# INLINE pretty #-}

{-----------------------------------------------------------------------
--  IP addr
-----------------------------------------------------------------------}

class IPAddress i where
  toHostAddr :: i -> Either HostAddress HostAddress6

instance IPAddress IPv4 where
  toHostAddr = Left . toHostAddress
  {-# INLINE toHostAddr #-}

instance IPAddress IPv6 where
  toHostAddr = Right . toHostAddress6
  {-# INLINE toHostAddr #-}

instance IPAddress IP where
  toHostAddr (IPv4 ip) = toHostAddr ip
  toHostAddr (IPv6 ip) = toHostAddr ip
  {-# INLINE toHostAddr #-}

deriving instance Typeable IP
deriving instance Typeable IPv4
deriving instance Typeable IPv6

ipToBEncode :: Show i => i -> BValue
ipToBEncode ip = BString $ BS8.pack $ show ip
{-# INLINE ipToBEncode #-}

ipFromBEncode :: Read a => BValue -> BS.Result a
ipFromBEncode (BString (BS8.unpack -> ipStr))
  | Just ip <- readMaybe (ipStr) = pure ip
  |         otherwise            = decodingError $ "IP: " ++ ipStr
ipFromBEncode _    = decodingError $ "IP: addr should be a bstring"

instance BEncode IP where
  toBEncode   = ipToBEncode
  {-# INLINE toBEncode #-}
  fromBEncode = ipFromBEncode
  {-# INLINE fromBEncode #-}

instance BEncode IPv4 where
  toBEncode   = ipToBEncode
  {-# INLINE toBEncode #-}
  fromBEncode = ipFromBEncode
  {-# INLINE fromBEncode #-}

instance BEncode IPv6 where
  toBEncode   = ipToBEncode
  {-# INLINE toBEncode #-}
  fromBEncode = ipFromBEncode
  {-# INLINE fromBEncode #-}

-- | When 'get'ing an IP it must be 'isolate'd to the appropriate
-- number of bytes since we have no other way of telling which
-- address type we are trying to parse
instance Serialize IP where
    put (IPv4 ip) = put ip
    put (IPv6 ip) = put ip

    get = do
      n <- remaining
      case n of
        4  -> IPv4 <$> get
        16 -> IPv6 <$> get
        _ -> fail "Wrong number of bytes remaining to parse IP"

instance Serialize IPv4 where
    put = putWord32host    .  toHostAddress
    get = fromHostAddress <$> getWord32host

instance Serialize IPv6 where
    put ip = put $ toHostAddress6 ip
    get = fromHostAddress6 <$> get

instance Pretty IPv4 where
  pretty = PP.text . show
  {-# INLINE pretty #-}

instance Pretty IPv6 where
  pretty = PP.text . show
  {-# INLINE pretty #-}

instance Pretty IP where
  pretty = PP.text . show
  {-# INLINE pretty #-}

instance Hashable IPv4 where
  hashWithSalt = hashUsing toHostAddress
  {-# INLINE hashWithSalt #-}

instance Hashable IPv6 where
  hashWithSalt s a = hashWithSalt s (toHostAddress6 a)

instance Hashable IP where
  hashWithSalt s (IPv4 h) = hashWithSalt s h
  hashWithSalt s (IPv6 h) = hashWithSalt s h

{-----------------------------------------------------------------------
--  Peer addr
-----------------------------------------------------------------------}
-- TODO check semantic of ord and eq instances

-- | Peer address info normally extracted from peer list or peer
-- compact list encoding.
data PeerAddr a = PeerAddr
  { peerId   :: !(Maybe PeerId)

    -- | This is usually 'IPv4', 'IPv6', 'IP' or unresolved
    -- 'HostName'.
  , peerHost :: !a

    -- | The port the peer listenning for incoming P2P sessions.
  , peerPort :: {-# UNPACK #-} !PortNumber
  } deriving (Show, Eq, Ord, Typeable, Functor)

peer_ip_key, peer_id_key, peer_port_key :: BKey
peer_ip_key   = "ip"
peer_id_key   = "peer id"
peer_port_key = "port"

-- | The tracker's 'announce response' compatible encoding.
instance (Typeable a, BEncode a) => BEncode (PeerAddr a) where
  toBEncode PeerAddr {..} = toDict $
       peer_ip_key   .=! peerHost
    .: peer_id_key   .=? peerId
    .: peer_port_key .=! peerPort
    .: endDict

  fromBEncode = fromDict $ do
    peerAddr <$>! peer_ip_key
             <*>? peer_id_key
             <*>! peer_port_key
    where
      peerAddr = flip PeerAddr

-- | The tracker's 'compact peer list' compatible encoding. The
-- 'peerId' is always 'Nothing'.
--
--   For more info see: <http://www.bittorrent.org/beps/bep_0023.html>
--
-- TODO: test byte order
instance (Serialize a) => Serialize (PeerAddr a) where
  put PeerAddr {..} = put peerHost >> put peerPort
  get = PeerAddr Nothing <$> get <*> get

-- | @127.0.0.1:6881@
instance Default (PeerAddr IPv4) where
  def = "127.0.0.1:6881"

-- | @127.0.0.1:6881@
instance Default (PeerAddr IP) where
  def = IPv4 <$> def

-- | Example:
--
--   @peerPort \"127.0.0.1:6881\" == 6881@
--
instance IsString (PeerAddr IPv4) where
  fromString str
    | [hostAddrStr, portStr] <- splitWhen (== ':') str
    , Just hostAddr <- readMaybe hostAddrStr
    , Just portNum  <- toEnum <$> readMaybe portStr
                = PeerAddr Nothing hostAddr portNum
    | otherwise = error $ "fromString: unable to parse (PeerAddr IPv4): " ++ str

instance Read (PeerAddr IPv4) where
  readsPrec i = RP.readP_to_S $ do
    ipv4 <- RP.readS_to_P (readsPrec i)
    _    <- RP.char ':'
    port <- toEnum <$> RP.readS_to_P (readsPrec i)
    return $ PeerAddr Nothing ipv4 port

readsIPv6_port :: String -> [((IPv6, PortNumber), String)]
readsIPv6_port = RP.readP_to_S $ do
  ip <- RP.char '[' *> (RP.readS_to_P reads) <* RP.char ']'
  _ <- RP.char ':'
  port <- toEnum <$> read <$> (RP.many1 $ RP.satisfy isDigit) <* RP.eof
  return (ip,port)

instance IsString (PeerAddr IPv6) where
  fromString str
    | [((ip,port),"")] <- readsIPv6_port str =
        PeerAddr Nothing ip port
    | otherwise = error $ "fromString: unable to parse (PeerAddr IPv6): " ++ str

instance IsString (PeerAddr IP) where
  fromString str
    | '[' `L.elem` str = IPv6 <$> fromString str
    |      otherwise   = IPv4 <$> fromString str

-- | fingerprint + "at" + dotted.host.inet.addr:port
-- TODO: instances for IPv6, HostName
instance Pretty a => Pretty (PeerAddr a) where
  pretty PeerAddr {..}
    | Just pid <- peerId = pretty (fingerprint pid) <+> "at" <+> paddr
    |     otherwise      = paddr
    where
      paddr = pretty peerHost <> ":" <> text (show peerPort)

instance Hashable a => Hashable (PeerAddr a) where
  hashWithSalt s PeerAddr {..} =
    s `hashWithSalt` peerId `hashWithSalt` peerHost `hashWithSalt` peerPort

-- | Ports typically reserved for bittorrent P2P listener.
defaultPorts :: [PortNumber]
defaultPorts =  [6881..6889]

_resolvePeerAddr :: (IPAddress i) => PeerAddr HostName -> PeerAddr i
_resolvePeerAddr = undefined

_peerSockAddr :: PeerAddr IP -> (Family, SockAddr)
_peerSockAddr PeerAddr {..} =
    case peerHost of
          IPv4 ipv4 ->
              (AF_INET, SockAddrInet peerPort (toHostAddress  ipv4))
          IPv6 ipv6 ->
              (AF_INET6, SockAddrInet6 peerPort 0 (toHostAddress6 ipv6) 0)

peerSockAddr :: PeerAddr IP -> SockAddr
peerSockAddr = snd . _peerSockAddr

-- | Create a socket connected to the address specified in a peerAddr
peerSocket :: SocketType -> PeerAddr IP -> IO Socket
peerSocket socketType pa = do
    let (family, addr) = _peerSockAddr pa
    sock <- socket family socketType defaultProtocol
    connect sock addr
    return sock

{-----------------------------------------------------------------------
--  Node info
-----------------------------------------------------------------------}
--   $node-info
--
--   A \"node\" is a client\/server listening on a UDP port
--   implementing the distributed hash table protocol. The DHT is
--   composed of nodes and stores the location of peers. BitTorrent
--   clients include a DHT node, which is used to contact other nodes
--   in the DHT to get the location of peers to download from using
--   the BitTorrent protocol.

-- TODO more compact representation ('ShortByteString's?)

-- | Each node has a globally unique identifier known as the \"node
-- ID.\"
--
--   Normally, /this/ node id should be saved between invocations
--   of the client software.
newtype NodeId = NodeId ByteString
  deriving (Show, Eq, Ord, BEncode, Typeable)

nodeIdSize :: Int
nodeIdSize = 20

-- | Meaningless node id, for testing purposes only.
instance Default NodeId where
  def = NodeId (BS.replicate nodeIdSize 0)

instance Serialize NodeId where
  get = NodeId <$> getByteString nodeIdSize
  {-# INLINE get #-}
  put (NodeId bs) = putByteString bs
  {-# INLINE put #-}

-- | ASCII encoded.
instance IsString NodeId where
  fromString str
    | L.length str == nodeIdSize = NodeId (fromString str)
    |           otherwise = error "fromString: invalid NodeId length"
  {-# INLINE fromString #-}

-- | base16 encoded.
instance Pretty NodeId where
  pretty (NodeId nid) = PP.text $ BC.unpack $ Base16.encode nid

-- | Test if the nth bit is set.
testIdBit :: NodeId -> Word -> Bool
testIdBit (NodeId bs) i
  | fromIntegral i < nodeIdSize * 8
  , (q, r) <- quotRem (fromIntegral i) 8
  = testBit (BS.index bs q) r
  |     otherwise      = False
{-# INLINE testIdBit #-}

-- TODO WARN is the 'system' random suitable for this?
-- | Generate random NodeID used for the entire session.
--   Distribution of ID's should be as uniform as possible.
--
genNodeId :: IO NodeId
genNodeId = NodeId <$> getEntropy nodeIdSize

------------------------------------------------------------------------

-- | In Kademlia, the distance metric is XOR and the result is
-- interpreted as an unsigned integer.
newtype NodeDistance = NodeDistance BS.ByteString
  deriving (Eq, Ord)

instance Pretty NodeDistance where
  pretty (NodeDistance bs) = foldMap bitseq $ BS.unpack bs
    where
      listBits w = L.map (testBit w) (L.reverse [0..bitSize w - 1])
      bitseq = foldMap (int . fromEnum) . listBits

-- | distance(A,B) = |A xor B| Smaller values are closer.
distance :: NodeId -> NodeId -> NodeDistance
distance (NodeId a) (NodeId b) = NodeDistance (BS.pack (BS.zipWith xor a b))

------------------------------------------------------------------------

data NodeAddr a = NodeAddr
  { nodeHost ::                !a
  , nodePort :: {-# UNPACK #-} !PortNumber
  } deriving (Eq, Typeable, Functor)

instance Show a => Show (NodeAddr a) where
  showsPrec i NodeAddr {..}
    = showsPrec i nodeHost <> showString ":" <> showsPrec i nodePort

instance Read (NodeAddr IPv4) where
  readsPrec i x = [ (fromPeerAddr a, s) | (a, s) <- readsPrec i x ]

-- | @127.0.0.1:6882@
instance Default (NodeAddr IPv4) where
  def = "127.0.0.1:6882"

-- | KRPC compatible encoding.
instance Serialize a => Serialize (NodeAddr a) where
  get = NodeAddr <$> get <*> get
  {-# INLINE get #-}
  put NodeAddr {..} = put nodeHost >> put nodePort
  {-# INLINE put #-}

-- | Torrent file compatible encoding.
instance BEncode a => BEncode (NodeAddr a) where
  toBEncode NodeAddr {..} = toBEncode (nodeHost, nodePort)
  {-# INLINE toBEncode #-}
  fromBEncode b = uncurry NodeAddr <$> fromBEncode b
  {-# INLINE fromBEncode #-}

instance Hashable a => Hashable (NodeAddr a) where
  hashWithSalt s NodeAddr {..} = hashWithSalt s (nodeHost, nodePort)
  {-# INLINE hashWithSalt #-}

instance Pretty ip => Pretty (NodeAddr ip) where
  pretty NodeAddr {..} = pretty nodeHost <> ":" <> pretty nodePort

-- | Example:
--
--   @nodePort \"127.0.0.1:6881\" == 6881@
--
instance IsString (NodeAddr IPv4) where
  fromString = fromPeerAddr . fromString

fromPeerAddr :: PeerAddr a -> NodeAddr a
fromPeerAddr PeerAddr {..} = NodeAddr
  { nodeHost = peerHost
  , nodePort = peerPort
  }

------------------------------------------------------------------------

data NodeInfo a = NodeInfo
  { nodeId   :: !NodeId
  , nodeAddr :: !(NodeAddr a)
  } deriving (Show, Eq, Functor)

instance Eq a => Ord (NodeInfo a) where
  compare = comparing nodeId

-- | KRPC 'compact list' compatible encoding: contact information for
-- nodes is encoded as a 26-byte string. Also known as "Compact node
-- info" the 20-byte Node ID in network byte order has the compact
-- IP-address/port info concatenated to the end.
instance Serialize a => Serialize (NodeInfo a) where
  get = NodeInfo <$> get <*> get
  put NodeInfo {..} = put nodeId >> put nodeAddr

instance Pretty ip => Pretty (NodeInfo ip) where
  pretty NodeInfo {..} = pretty nodeId <> "@(" <> pretty nodeAddr <> ")"

instance Pretty ip => Pretty [NodeInfo ip] where
  pretty = PP.vcat . PP.punctuate "," . L.map pretty

-- | Order by closeness: nearest nodes first.
rank :: Eq ip => NodeId -> [NodeInfo ip] -> [NodeInfo ip]
rank nid = L.sortBy (comparing (distance nid . nodeId))

{-----------------------------------------------------------------------
-- Fingerprint
-----------------------------------------------------------------------}
--   $fingerprint
--
--   'Fingerprint' is used to identify the client implementation and
--   version which also contained in 'Peer'. For exsample first 6
--   bytes of peer id of this this library are @-HS0100-@ while for
--   mainline we have @M4-3-6--@.  We could extract this info and
--   print in human-friendly form: this is useful for debugging and
--   logging.
--
--   For more information see:
--   <http://bittorrent.org/beps/bep_0020.html>
--
--
--   NOTE: Do /not/ use this information to control client
--   capabilities (such as supported enchancements), this should be
--   done using 'Network.BitTorrent.Extension'!
--

-- TODO FIXME
version :: Version
version = Version [0, 0, 0, 3] []

-- | List of registered client versions + 'IlibHSbittorrent' (this
-- package) + 'IUnknown' (for not recognized software). All names are
-- prefixed by \"I\" because some of them starts from lowercase letter
-- but that is not a valid Haskell constructor name.
--
data ClientImpl =
   IUnknown

 | IMainline

 | IABC
 | IOspreyPermaseed
 | IBTQueue
 | ITribler
 | IShadow
 | IBitTornado

-- UPnP(!) Bit Torrent !???
-- 'U' - UPnP NAT Bit Torrent
 | IBitLord
 | IOpera
 | IMLdonkey

 | IAres
 | IArctic
 | IAvicora
 | IBitPump
 | IAzureus
 | IBitBuddy
 | IBitComet
 | IBitflu
 | IBTG
 | IBitRocket
 | IBTSlave
 | IBittorrentX
 | IEnhancedCTorrent
 | ICTorrent
 | IDelugeTorrent
 | IPropagateDataClient
 | IEBit
 | IElectricSheep
 | IFoxTorrent
 | IGSTorrent
 | IHalite
 | IlibHSbittorrent
 | IHydranode
 | IKGet
 | IKTorrent
 | ILH_ABC
 | ILphant
 | ILibtorrent
 | ILibTorrent
 | ILimeWire
 | IMonoTorrent
 | IMooPolice
 | IMiro
 | IMoonlightTorrent
 | INetTransport
 | IPando
 | IqBittorrent
 | IQQDownload
 | IQt4TorrentExample
 | IRetriever
 | IShareaza
 | ISwiftbit
 | ISwarmScope
 | ISymTorrent
 | Isharktorrent
 | ITorrentDotNET
 | ITransmission
 | ITorrentstorm
 | ITuoTu
 | IuLeecher
 | IuTorrent
 | IVagaa
 | IBitLet
 | IFireTorrent
 | IXunlei
 | IXanTorrent
 | IXtorrent
 | IZipTorrent
   deriving (Show, Eq, Ord, Enum, Bounded)

parseImpl :: ByteString -> ClientImpl
parseImpl = f . BC.unpack
 where
  f "AG" = IAres
  f "A~" = IAres
  f "AR" = IArctic
  f "AV" = IAvicora
  f "AX" = IBitPump
  f "AZ" = IAzureus
  f "BB" = IBitBuddy
  f "BC" = IBitComet
  f "BF" = IBitflu
  f "BG" = IBTG
  f "BR" = IBitRocket
  f "BS" = IBTSlave
  f "BX" = IBittorrentX
  f "CD" = IEnhancedCTorrent
  f "CT" = ICTorrent
  f "DE" = IDelugeTorrent
  f "DP" = IPropagateDataClient
  f "EB" = IEBit
  f "ES" = IElectricSheep
  f "FT" = IFoxTorrent
  f "GS" = IGSTorrent
  f "HL" = IHalite
  f "HS" = IlibHSbittorrent
  f "HN" = IHydranode
  f "KG" = IKGet
  f "KT" = IKTorrent
  f "LH" = ILH_ABC
  f "LP" = ILphant
  f "LT" = ILibtorrent
  f "lt" = ILibTorrent
  f "LW" = ILimeWire
  f "MO" = IMonoTorrent
  f "MP" = IMooPolice
  f "MR" = IMiro
  f "ML" = IMLdonkey
  f "MT" = IMoonlightTorrent
  f "NX" = INetTransport
  f "PD" = IPando
  f "qB" = IqBittorrent
  f "QD" = IQQDownload
  f "QT" = IQt4TorrentExample
  f "RT" = IRetriever
  f "S~" = IShareaza
  f "SB" = ISwiftbit
  f "SS" = ISwarmScope
  f "ST" = ISymTorrent
  f "st" = Isharktorrent
  f "SZ" = IShareaza
  f "TN" = ITorrentDotNET
  f "TR" = ITransmission
  f "TS" = ITorrentstorm
  f "TT" = ITuoTu
  f "UL" = IuLeecher
  f "UT" = IuTorrent
  f "VG" = IVagaa
  f "WT" = IBitLet
  f "WY" = IFireTorrent
  f "XL" = IXunlei
  f "XT" = IXanTorrent
  f "XX" = IXtorrent
  f "ZT" = IZipTorrent
  f _    = IUnknown

-- | Used to represent a not recognized implementation
instance Default ClientImpl where
  def = IUnknown
  {-# INLINE def #-}

-- | Example: @\"BitLet\" == 'IBitLet'@
instance IsString ClientImpl where
  fromString str
    | Just impl <- L.lookup str alist = impl
    | otherwise = error $ "fromString: not recognized " ++ str
    where
      alist = L.map mk [minBound..maxBound]
      mk  x = (L.tail $ show x, x)

-- | Example: @pretty 'IBitLet' == \"IBitLet\"@
instance Pretty ClientImpl where
  pretty = text . L.tail . show

-- | Just the '0' version.
instance Default Version where
  def = Version [0] []
  {-# INLINE def #-}

-- | For dot delimited version strings.
--   Example: @fromString \"0.1.0.2\" == Version [0, 1, 0, 2]@
--
instance IsString Version where
  fromString str
    | Just nums <- chunkNums str = Version nums []
    | otherwise = error $ "fromString: invalid version string " ++ str
    where
      chunkNums = sequence . L.map readMaybe . L.linesBy ('.' ==)

instance Pretty Version where
  pretty = text . showVersion

-- | The all sensible infomation that can be obtained from a peer
-- identifier or torrent /createdBy/ field.
data Fingerprint = Fingerprint
  { ciImpl    :: ClientImpl
  , ciVersion :: Version
  } deriving (Show, Eq, Ord)

-- | Unrecognized client implementation.
instance Default Fingerprint where
  def = Fingerprint def def
  {-# INLINE def #-}

-- | Example: @\"BitComet-1.2\" == ClientInfo IBitComet (Version [1, 2] [])@
instance IsString Fingerprint where
  fromString str
    | _ : ver <- _ver = Fingerprint (fromString impl) (fromString ver)
    | otherwise = error $ "fromString: invalid client info string" ++ str
    where
      (impl, _ver) = L.span ((/=) '-') str

instance Pretty Fingerprint where
  pretty Fingerprint {..} = pretty ciImpl <+> "version" <+> pretty ciVersion

-- | Fingerprint of this (the bittorrent library) package. Normally,
-- applications should introduce its own fingerprints, otherwise they
-- can use 'libFingerprint' value.
--
libFingerprint :: Fingerprint
libFingerprint =  Fingerprint IlibHSbittorrent version

-- | HTTP user agent of this (the bittorrent library) package. Can be
-- used in HTTP tracker requests.
libUserAgent :: String
libUserAgent = render (pretty IlibHSbittorrent <> "/" <> pretty version)

{-----------------------------------------------------------------------
--  For torrent file
-----------------------------------------------------------------------}
-- TODO collect information about createdBy torrent field
{-
renderImpl :: ClientImpl -> Text
renderImpl = T.pack . L.tail . show

renderVersion :: Version -> Text
renderVersion = undefined

renderClientInfo :: ClientInfo -> Text
renderClientInfo ClientInfo {..} = renderImpl ciImpl <> "/" <> renderVersion ciVersion

parseClientInfo :: Text -> ClientImpl
parseClientInfo t = undefined
-}
{-
-- code used for generation; remove it later on

mkEnumTyDef :: NM -> String
mkEnumTyDef = unlines . map (" | I" ++) . nub . map snd

mkPars :: NM -> String
mkPars = unlines . map (\(code, impl) -> "  f \"" ++ code ++ "\" = " ++ "I" ++ impl)

type NM = [(String, String)]
nameMap :: NM
nameMap =
 [ ("AG", "Ares")
 , ("A~", "Ares")
 , ("AR", "Arctic")
 , ("AV", "Avicora")
 , ("AX", "BitPump")
 , ("AZ", "Azureus")
 , ("BB", "BitBuddy")
 , ("BC", "BitComet")
 , ("BF", "Bitflu")
 , ("BG", "BTG")
 , ("BR", "BitRocket")
 , ("BS", "BTSlave")
 , ("BX", "BittorrentX")
 , ("CD", "EnhancedCTorrent")
 , ("CT", "CTorrent")
 , ("DE", "DelugeTorrent")
 , ("DP", "PropagateDataClient")
 , ("EB", "EBit")
 , ("ES", "ElectricSheep")
 , ("FT", "FoxTorrent")
 , ("GS", "GSTorrent")
 , ("HL", "Halite")
 , ("HS", "libHSnetwork_bittorrent")
 , ("HN", "Hydranode")
 , ("KG", "KGet")
 , ("KT", "KTorrent")
 , ("LH", "LH_ABC")
 , ("LP", "Lphant")
 , ("LT", "Libtorrent")
 , ("lt", "LibTorrent")
 , ("LW", "LimeWire")
 , ("MO", "MonoTorrent")
 , ("MP", "MooPolice")
 , ("MR", "Miro")
 , ("MT", "MoonlightTorrent")
 , ("NX", "NetTransport")
 , ("PD", "Pando")
 , ("qB", "qBittorrent")
 , ("QD", "QQDownload")
 , ("QT", "Qt4TorrentExample")
 , ("RT", "Retriever")
 , ("S~", "Shareaza")
 , ("SB", "Swiftbit")
 , ("SS", "SwarmScope")
 , ("ST", "SymTorrent")
 , ("st", "sharktorrent")
 , ("SZ", "Shareaza")
 , ("TN", "TorrentDotNET")
 , ("TR", "Transmission")
 , ("TS", "Torrentstorm")
 , ("TT", "TuoTu")
 , ("UL", "uLeecher")
 , ("UT", "uTorrent")
 , ("VG", "Vagaa")
 , ("WT", "BitLet")
 , ("WY", "FireTorrent")
 , ("XL", "Xunlei")
 , ("XT", "XanTorrent")
 , ("XX", "Xtorrent")
 , ("ZT", "ZipTorrent")
 ]
-}

-- TODO use regexps

-- | Tries to extract meaningful information from peer ID bytes. If
-- peer id uses unknown coding style then client info returned is
-- 'def'.
--
fingerprint :: PeerId -> Fingerprint
fingerprint pid = either (const def) id $ runGet getCI (getPeerId pid)
  where
    getCI    = do
      leading <- BS.w2c <$> getWord8
      case leading of
        '-' -> Fingerprint <$> getAzureusImpl <*> getAzureusVersion
        'M' -> Fingerprint <$> pure IMainline <*> getMainlineVersion
        'e' -> Fingerprint <$> getBitCometImpl <*> getBitCometVersion
        'F' -> Fingerprint <$> getBitCometImpl <*> getBitCometVersion
        c   -> do
          c1 <- w2c <$> S.lookAhead getWord8
          if c1 == 'P'
            then do
                 _ <- getWord8
                 Fingerprint <$> pure IOpera            <*> getOperaVersion
            else Fingerprint <$> pure (getShadowImpl c) <*> getShadowVersion

    getMainlineVersion = do
      str <- BC.unpack <$> getByteString 7
      let mnums = L.filter (not . L.null) $ L.linesBy ('-' ==) str
      return $ Version (fromMaybe [] $ sequence $ L.map readMaybe mnums) []

    getAzureusImpl    = parseImpl <$> getByteString 2
    getAzureusVersion = mkVer     <$> getByteString 4
      where
        mkVer bs = Version [fromMaybe 0 $ readMaybe $ BC.unpack bs] []

    getBitCometImpl = do
      bs <- getByteString 3
      S.lookAhead $ do
        _  <- getByteString 2
        lr <- getByteString 4
        return $
          if lr == "LORD" then IBitLord  else
          if bs == "UTB"  then IBitComet else
          if bs == "xbc"  then IBitComet else def

    getBitCometVersion = do
      x <- getWord8
      y <- getWord8
      return $ Version [fromIntegral x, fromIntegral y] []

    getOperaVersion = do
      str <- BC.unpack <$> getByteString 4
      return $ Version [fromMaybe 0 $ readMaybe str] []

    getShadowImpl 'A' = IABC
    getShadowImpl 'O' = IOspreyPermaseed
    getShadowImpl 'Q' = IBTQueue
    getShadowImpl 'R' = ITribler
    getShadowImpl 'S' = IShadow
    getShadowImpl 'T' = IBitTornado
    getShadowImpl  _  = IUnknown

    decodeShadowVerNr :: Char -> Maybe Int
    decodeShadowVerNr c
      | '0' < c && c <= '9' = Just  (fromEnum c - fromEnum '0')
      | 'A' < c && c <= 'Z' = Just ((fromEnum c - fromEnum 'A') + 10)
      | 'a' < c && c <= 'z' = Just ((fromEnum c - fromEnum 'a') + 36)
      |        otherwise    = Nothing

    getShadowVersion = do
      str <- BC.unpack <$> getByteString 5
      return $ Version (catMaybes $ L.map decodeShadowVerNr str) []
