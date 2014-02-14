-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Normally peer to peer communication consisting of the following
--   steps:
--
--   * In order to establish the connection between peers we should
--   send 'Handshake' message. The 'Handshake' is a required message
--   and must be the first message transmitted by the peer to the
--   another peer. Another peer should reply with a handshake as well.
--
--   * Next peer might sent bitfield message, but might not. In the
--   former case we should update bitfield peer have. Again, if we
--   have some pieces we should send bitfield. Normally bitfield
--   message should sent after the handshake message.
--
--   * Regular exchange messages. TODO docs
--
--   For more high level API see "Network.BitTorrent.Exchange" module.
--
--   For more infomation see:
--   <https://wiki.theory.org/BitTorrentSpecification#Peer_wire_protocol_.28TCP.29>
--
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS  -fno-warn-orphans          #-}
module Network.BitTorrent.Exchange.Message
       ( -- * Capabilities
         Capabilities (..)
       , Extension (..)
       , Caps

         -- * Handshake
       , ProtocolName
       , Handshake(..)
       , defaultHandshake
       , handshakeSize
       , handshakeMaxSize
       , handshakeStats

         -- * Stats
       , ByteCount
       , ByteStats   (..)
       , byteLength

         -- * Messages
       , Message        (..)
       , defaultKeepAliveTimeout
       , defaultKeepAliveInterval
       , PeerMessage    (..)

         -- ** Core messages
       , StatusUpdate   (..)
       , Available      (..)
       , Transfer       (..)
       , defaultRequestQueueLength

         -- ** Fast extension
       , FastMessage    (..)

         -- ** Extension protocol
       , ExtendedMessage   (..)

         -- *** Capabilities
       , ExtendedExtension (..)
       , ExtendedCaps      (..)

         -- *** Handshake
       , ExtendedHandshake (..)
       , defaultQueueLength
       , nullExtendedHandshake

         -- *** Metadata
       , ExtendedMetadata  (..)
       , metadataPieceSize
       , defaultMetadataFactor
       , defaultMaxInfoDictSize
       , isLastPiece
       , isValidPiece
       ) where

import Control.Applicative
import Control.Arrow ((&&&), (***))
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8 as BS
import Data.BEncode as BE
import Data.BEncode.BDict as BE
import Data.BEncode.Internal as BE (ppBEncode, parser)
import Data.BEncode.Types    (BDict)
import Data.Bits
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy  as BL
import Data.Default
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Serialize as S
import Data.String
import Data.Text as T
import Data.Typeable
import Data.Word
import Data.IP
import Network
import Network.Socket hiding (KeepAlive)
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class

import Data.Torrent.Bitfield
import Data.Torrent.InfoHash
import qualified Data.Torrent.Piece as P
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Block

{-----------------------------------------------------------------------
--  Capabilities
-----------------------------------------------------------------------}

-- |
class Capabilities caps where
  type Ext caps :: *

  -- | Pack extensions to caps.
  toCaps   :: [Ext caps] -> caps

  -- | Unpack extensions from caps.
  fromCaps :: caps -> [Ext caps]

  -- | Check if an extension is a member of the specified set.
  allowed  :: Ext caps -> caps -> Bool

ppCaps :: Capabilities caps => Pretty (Ext caps) => caps -> Doc
ppCaps = hcat . punctuate ", " . L.map pretty . fromCaps

{-----------------------------------------------------------------------
--  Extensions
-----------------------------------------------------------------------}

-- | Enumeration of message extension protocols.
--
--   For more info see: <http://www.bittorrent.org/beps/bep_0004.html>
--
data Extension
  = ExtDHT      -- ^ BEP 5:  allow to send PORT messages.
  | ExtFast     -- ^ BEP 6:  allow to send FAST messages.
  | ExtExtended -- ^ BEP 10: allow to send the extension protocol messages.
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Full extension names, suitable for logging.
instance Pretty Extension where
  pretty ExtDHT      = "Distributed Hash Table Protocol"
  pretty ExtFast     = "Fast Extension"
  pretty ExtExtended = "Extension Protocol"

-- | Extension bitmask as specified by BEP 4.
extMask :: Extension -> Word64
extMask ExtDHT      = 0x01
extMask ExtFast     = 0x04
extMask ExtExtended = 0x100000

{-----------------------------------------------------------------------
--  Capabilities
-----------------------------------------------------------------------}

-- | Capabilities is a set of 'Extension's usually sent in 'Handshake'
-- messages.
newtype Caps = Caps Word64
  deriving (Show, Eq)

-- | Render set of extensions as comma separated list.
instance Pretty Caps where
  pretty = ppCaps
  {-# INLINE pretty #-}

-- | The empty set.
instance Default Caps where
  def = Caps 0
  {-# INLINE def #-}

-- | Monoid under intersection. 'mempty' includes all known extensions.
instance Monoid Caps where
  mempty = toCaps [minBound .. maxBound]
  {-# INLINE mempty #-}

  mappend (Caps a) (Caps b) = Caps (a .&. b)
  {-# INLINE mappend #-}

-- | 'Handshake' compatible encoding.
instance Serialize Caps where
  put (Caps caps) = S.putWord64be caps
  {-# INLINE put #-}

  get = Caps <$> S.getWord64be
  {-# INLINE get #-}

instance Capabilities Caps where
  type Ext Caps = Extension

  allowed e (Caps caps) = (extMask e .&. caps) /= 0
  {-# INLINE allowed #-}

  toCaps        = Caps . L.foldr (.|.) 0 . L.map extMask
  fromCaps caps = L.filter (`allowed` caps) [minBound..maxBound]

{-----------------------------------------------------------------------
    Handshake
-----------------------------------------------------------------------}

maxProtocolNameSize :: Word8
maxProtocolNameSize = maxBound

-- | The protocol name is used to identify to the local peer which
-- version of BTP the remote peer uses.
newtype ProtocolName = ProtocolName BS.ByteString
  deriving (Eq, Ord, Typeable)

-- | In BTP/1.0 the name is 'BitTorrent protocol'. If this string is
-- different from the local peers own protocol name, then the
-- connection is to be dropped.
instance Default ProtocolName where
  def = ProtocolName "BitTorrent protocol"

instance Show ProtocolName where
  show (ProtocolName bs) = show bs

instance Pretty ProtocolName where
  pretty (ProtocolName bs) = PP.text $ BC.unpack bs

instance IsString ProtocolName where
  fromString str
    | L.length str <= fromIntegral maxProtocolNameSize
    = ProtocolName (fromString str)
    | otherwise = error $ "fromString: ProtocolName too long: " ++ str

instance Serialize ProtocolName where
  put (ProtocolName bs) = do
    putWord8 $ fromIntegral $ BS.length bs
    putByteString bs

  get = do
    len  <- getWord8
    bs   <- getByteString $ fromIntegral len
    return (ProtocolName bs)

-- | Handshake message is used to exchange all information necessary
-- to establish connection between peers.
--
data Handshake = Handshake {
    -- | Identifier of the protocol. This is usually equal to 'def'.
    hsProtocol    :: ProtocolName

    -- | Reserved bytes used to specify supported BEP's.
  , hsReserved    :: Caps

    -- | Info hash of the info part of the metainfo file. that is
    -- transmitted in tracker requests. Info hash of the initiator
    -- handshake and response handshake should match, otherwise
    -- initiator should break the connection.
    --
  , hsInfoHash    :: InfoHash

    -- | Peer id of the initiator. This is usually the same peer id
    -- that is transmitted in tracker requests.
    --
  , hsPeerId      :: PeerId

  } deriving (Show, Eq)

instance Serialize Handshake where
  put Handshake {..} = do
    put hsProtocol
    put hsReserved
    put hsInfoHash
    put hsPeerId
  get = Handshake <$> get <*> get <*> get <*> get

-- | Show handshake protocol string, caps and fingerprint.
instance Pretty Handshake where
  pretty Handshake {..}
    = pretty hsProtocol           $$
      pretty hsReserved           $$
      pretty (fingerprint hsPeerId)

-- | Get handshake message size in bytes from the length of protocol
-- string.
handshakeSize :: Word8 -> Int
handshakeSize n = 1 + fromIntegral n + 8 + 20 + 20

-- | Maximum size of handshake message in bytes.
handshakeMaxSize :: Int
handshakeMaxSize = handshakeSize maxProtocolNameSize

-- | Handshake with default protocol string and reserved bitmask.
defaultHandshake :: InfoHash -> PeerId -> Handshake
defaultHandshake = Handshake def def

handshakeStats :: Handshake -> ByteStats
handshakeStats (Handshake (ProtocolName bs) _ _ _)
  = ByteStats 1 (BS.length bs + 8 + 20 + 20) 0

{-----------------------------------------------------------------------
--  Stats
-----------------------------------------------------------------------}

-- | Number of bytes.
type ByteCount = Int

-- | Summary of encoded message byte layout can be used to collect
-- stats about message flow in both directions. This data can be
-- retrieved using 'stats' function.
data ByteStats = ByteStats
  { -- | Number of bytes used to help encode 'control' and 'payload'
    -- bytes: message size, message ID's, etc
    overhead :: {-# UNPACK #-} !ByteCount

    -- | Number of bytes used to exchange peers state\/options: piece
    -- and block indexes, infohash, port numbers, peer ID\/IP, etc.
  , control  :: {-# UNPACK #-} !ByteCount

    -- | Number of payload bytes: torrent data blocks and infodict
    -- metadata.
  , payload  :: {-# UNPACK #-} !ByteCount
  } deriving Show

instance Pretty ByteStats where
  pretty s @ ByteStats {..} = fsep
    [ PP.int overhead, "overhead"
    , PP.int control,  "control"
    , PP.int payload,  "payload"
    , "bytes"
    ] $+$ fsep
    [ PP.int (byteLength s), "total bytes"
    ]

-- | Empty byte sequences.
instance Default ByteStats where
  def = ByteStats 0 0 0

-- | Monoid under addition.
instance Monoid ByteStats where
  mempty      = def
  mappend a b = ByteStats
    { overhead = overhead a + overhead b
    , control  = control  a + control  b
    , payload  = payload  a + payload  b
    }

-- | Sum of the all byte sequences.
byteLength :: ByteStats -> Int
byteLength ByteStats {..} = overhead + control + payload

{-----------------------------------------------------------------------
--  Regular messages
-----------------------------------------------------------------------}

-- | Messages which can be sent after handshaking. Minimal complete
-- definition: 'envelop'.
class PeerMessage a where
  -- | Construct a message to be /sent/. Note that if 'ExtendedCaps'
  -- do not contain mapping for this message the default
  -- 'ExtendedMessageId' is used.
  envelop :: ExtendedCaps -- ^ The /receiver/ extended capabilities;
          -> a            -- ^ An regular message;
          -> Message      -- ^ Enveloped message to sent.

  -- | Find out the extension this message belong to. Can be used to
  -- check if this message is allowed to send\/recv in current
  -- session.
  requires :: a -> Maybe Extension
  requires _ = Nothing

  -- | Get sizes of overhead\/control\/payload byte sequences of
  -- binary message representation without encoding message to binary
  -- bytestring.
  --
  --   This function should obey one law:
  --
  --     * 'byteLength' ('stats' msg) == 'BL.length' ('encode' msg)
  --
  stats :: a -> ByteStats
  stats _ = ByteStats 4 0 0

{-----------------------------------------------------------------------
--  Status messages
-----------------------------------------------------------------------}

-- | Notification that the sender have updated its
-- 'Network.BitTorrent.Exchange.Status.PeerStatus'.
data StatusUpdate
    -- | Notification that the sender will not upload data to the
    -- receiver until unchoking happen.
  = Choking    !Bool

    -- | Notification that the sender is interested (or not interested)
    -- in any of the receiver's data pieces.
  | Interested !Bool
    deriving (Show, Eq, Ord, Typeable)

instance Pretty StatusUpdate where
  pretty (Choking    False) = "not choking"
  pretty (Choking    True ) = "choking"
  pretty (Interested False) = "not interested"
  pretty (Interested True ) = "interested"

instance PeerMessage StatusUpdate where
  envelop _ = Status
  {-# INLINE envelop #-}

  stats _ = ByteStats 4 1 0
  {-# INLINE stats #-}

{-----------------------------------------------------------------------
--  Available messages
-----------------------------------------------------------------------}

-- | Messages used to inform receiver which pieces of the torrent
-- sender have.
data Available =
    -- | Zero-based index of a piece that has just been successfully
    -- downloaded and verified via the hash.
    Have    ! PieceIx

    -- | The bitfield message may only be sent immediately after the
    -- handshaking sequence is complete, and before any other message
    -- are sent. If client have no pieces then bitfield need not to be
    -- sent.
  | Bitfield !Bitfield
    deriving (Show, Eq)

instance Pretty Available where
  pretty (Have     ix ) = "Have"     <+> int ix
  pretty (Bitfield _  ) = "Bitfield"

instance PeerMessage Available where
  envelop _ = Available
  {-# INLINE envelop #-}

  stats (Have      _) = ByteStats (4 + 1) 4 0
  stats (Bitfield bf) = ByteStats (4 + 1) (q + trailing)  0
    where
      trailing = if r == 0 then 0 else 1
      (q, r) = quotRem (totalCount bf) 8

{-----------------------------------------------------------------------
--  Transfer messages
-----------------------------------------------------------------------}

-- | Messages used to transfer 'Block's.
data Transfer
    -- | Request for a particular block. If a client is requested a
    -- block that another peer do not have the peer might not answer
    -- at all.
  = Request ! BlockIx

    -- | Response to a request for a block.
  | Piece   !(Block BL.ByteString)

    -- | Used to cancel block requests. It is typically used during
    -- "End Game".
  | Cancel  !BlockIx
    deriving (Show, Eq)

instance Pretty Transfer where
  pretty (Request  ix ) = "Request"  <+> pretty ix
  pretty (Piece    blk) = "Piece"    <+> pretty blk
  pretty (Cancel   i  ) = "Cancel"   <+> pretty i

instance PeerMessage Transfer where
  envelop _ = Transfer
  {-# INLINE envelop #-}

  stats (Request _  ) = ByteStats (4 + 1) (3 * 4) 0
  stats (Piece   p  ) = ByteStats (4 + 1) (4 + 4 + blockSize p) 0
  stats (Cancel  _  ) = ByteStats (4 + 1) (3 * 4) 0

-- TODO increase
-- | Max number of pending 'Request's inflight.
defaultRequestQueueLength :: Int
defaultRequestQueueLength = 1

{-----------------------------------------------------------------------
--  Fast messages
-----------------------------------------------------------------------}

-- | BEP6 messages.
data FastMessage =
    -- | If a peer have all pieces it might send the 'HaveAll' message
    -- instead of 'Bitfield' message. Used to save bandwidth.
    HaveAll

    -- | If a peer have no pieces it might send 'HaveNone' message
    -- intead of 'Bitfield' message. Used to save bandwidth.
  | HaveNone

    -- | This is an advisory message meaning "you might like to
    -- download this piece." Used to avoid excessive disk seeks and
    -- amount of IO.
  | SuggestPiece  !PieceIx

    -- | Notifies a requesting peer that its request will not be
    -- satisfied.
  | RejectRequest !BlockIx

    -- | This is an advisory messsage meaning \"if you ask for this
    -- piece, I'll give it to you even if you're choked.\" Used to
    -- shorten starting phase.
  | AllowedFast   !PieceIx
    deriving (Show, Eq)

instance Pretty FastMessage where
  pretty (HaveAll          ) = "Have all"
  pretty (HaveNone         ) = "Have none"
  pretty (SuggestPiece  pix) = "Suggest"      <+> int    pix
  pretty (RejectRequest bix) = "Reject"       <+> pretty bix
  pretty (AllowedFast   pix) = "Allowed fast" <+> int    pix

instance PeerMessage FastMessage where
  envelop  _ = Fast
  {-# INLINE envelop #-}

  requires _ = Just ExtFast
  {-# INLINE requires #-}

  stats  HaveAll          = ByteStats 4 1  0
  stats  HaveNone         = ByteStats 4 1  0
  stats (SuggestPiece  _) = ByteStats 5 4  0
  stats (RejectRequest _) = ByteStats 5 12 0
  stats (AllowedFast   _) = ByteStats 5 4  0

{-----------------------------------------------------------------------
--  Extension protocol
-----------------------------------------------------------------------}

{-----------------------------------------------------------------------
--  Extended capabilities
-----------------------------------------------------------------------}

data ExtendedExtension
  = ExtMetadata -- ^ BEP 9: Extension for Peers to Send Metadata Files
    deriving (Show, Eq, Ord, Enum, Bounded, Typeable)

instance IsString ExtendedExtension where
  fromString = fromMaybe (error msg) . fromKey . fromString
    where
      msg = "fromString: could not parse ExtendedExtension"

instance Pretty ExtendedExtension where
  pretty ExtMetadata = "Extension for Peers to Send Metadata Files"

fromKey :: BKey -> Maybe ExtendedExtension
fromKey "ut_metadata" = Just ExtMetadata
fromKey _             = Nothing
{-# INLINE fromKey #-}

toKey :: ExtendedExtension -> BKey
toKey ExtMetadata = "ut_metadata"
{-# INLINE toKey #-}

type ExtendedMessageId = Word8

extId :: ExtendedExtension -> ExtendedMessageId
extId ExtMetadata = 1
{-# INLINE extId #-}

type ExtendedMap = Map ExtendedExtension ExtendedMessageId

-- | The extension IDs must be stored for every peer, because every
-- peer may have different IDs for the same extension.
--
newtype ExtendedCaps = ExtendedCaps { extendedCaps :: ExtendedMap }
  deriving (Show, Eq)

instance Pretty ExtendedCaps where
  pretty = ppCaps
  {-# INLINE pretty #-}

-- | The empty set.
instance Default ExtendedCaps where
  def = ExtendedCaps M.empty

-- | Monoid under intersection:
--
--     * The 'mempty' caps includes all known extensions;
--
--     * the 'mappend' operation is NOT commutative: it return message
-- id from the first caps for the extensions existing in both caps.
--
instance Monoid ExtendedCaps where
  mempty  = toCaps [minBound..maxBound]
  mappend (ExtendedCaps a) (ExtendedCaps b) =
    ExtendedCaps (M.intersection a b)

appendBDict ::  BDict -> ExtendedMap -> ExtendedMap
appendBDict (Cons key val xs) caps
  | Just  ext <- fromKey     key
  , Right eid <- fromBEncode val   = M.insert ext eid (appendBDict xs caps)
  |           otherwise            = appendBDict xs caps
appendBDict  Nil              caps = caps

-- | Handshake compatible encoding.
instance BEncode ExtendedCaps where
  toBEncode = BDict . BE.fromAscList . L.sortBy (comparing fst)
            . L.map (toKey *** toBEncode) . M.toList . extendedCaps

  fromBEncode (BDict bd) = pure $ ExtendedCaps $ appendBDict bd M.empty
  fromBEncode _          = decodingError "ExtendedCaps"

instance Capabilities ExtendedCaps where
  type Ext ExtendedCaps = ExtendedExtension

  toCaps = ExtendedCaps . M.fromList . L.map (id &&& extId)

  fromCaps = M.keys . extendedCaps
  {-# INLINE fromCaps #-}

  allowed e (ExtendedCaps caps) = M.member e caps
  {-# INLINE allowed #-}

remoteMessageId :: ExtendedExtension -> ExtendedCaps -> ExtendedMessageId
remoteMessageId ext = fromMaybe (extId ext) . M.lookup ext . extendedCaps

{-----------------------------------------------------------------------
--  Extended handshake
-----------------------------------------------------------------------}

-- | This message should be sent immediately after the standard
-- bittorrent handshake to any peer that supports this extension
-- protocol. Extended handshakes can be sent more than once, however
-- an implementation may choose to ignore subsequent handshake
-- messages.
--
data ExtendedHandshake = ExtendedHandshake
  { -- | If this peer has an IPv4 interface, this is the compact
    -- representation of that address.
    ehsIPv4        :: Maybe HostAddress

    -- | If this peer has an IPv6 interface, this is the compact
    -- representation of that address.
  , ehsIPv6        :: Maybe HostAddress6

    -- | Dictionary of supported extension messages which maps names
    -- of extensions to an extended message ID for each extension
    -- message.
  , ehsCaps        :: ExtendedCaps

    -- | Size of 'Data.Torrent.InfoDict' in bytes. This field should
    -- be added if 'ExtMetadata' is enabled in current session /and/
    -- peer have the torrent file.
  , ehsMetadataSize :: Maybe Int

    -- | Local TCP /listen/ port. Allows each side to learn about the
    -- TCP port number of the other side.
  , ehsPort        :: Maybe PortNumber

    -- | Request queue the number of outstanding 'Request' messages
    -- this client supports without dropping any.
  , ehsQueueLength :: Maybe Int

    -- | Client name and version.
  , ehsVersion     :: Maybe Text

    -- | IP of the remote end
  , ehsYourIp      :: Maybe IP
  } deriving (Show, Eq, Typeable)

extHandshakeId :: ExtendedMessageId
extHandshakeId = 0

-- | Default 'Request' queue size.
defaultQueueLength :: Int
defaultQueueLength = 1

-- | All fields are empty.
instance Default ExtendedHandshake where
  def = ExtendedHandshake def def def def def def def def

instance Monoid ExtendedHandshake where
    mempty = def { ehsCaps = mempty }
    mappend old new = ExtendedHandshake {
      ehsCaps         = ehsCaps old <> ehsCaps new,
      ehsIPv4         = ehsIPv4 old         `mergeOld` ehsIPv4 new,
      ehsIPv6         = ehsIPv6 old         `mergeOld` ehsIPv6 new,
      ehsMetadataSize = ehsMetadataSize old `mergeNew` ehsMetadataSize new,
      ehsPort         = ehsPort old         `mergeOld` ehsPort new,
      ehsQueueLength  = ehsQueueLength old  `mergeNew` ehsQueueLength new,
      ehsVersion      = ehsVersion old      `mergeOld` ehsVersion new,
      ehsYourIp       = ehsYourIp old       `mergeOld` ehsYourIp new
    }
      where
        mergeOld mold mnew = mold <|> mnew
        mergeNew mold mnew = mnew <|> mold


instance BEncode ExtendedHandshake where
  toBEncode ExtendedHandshake {..} = toDict $
       "ipv4"   .=? (S.encode <$> ehsIPv4)
    .: "ipv6"   .=? (S.encode <$> ehsIPv6)
    .: "m"      .=! ehsCaps
    .: "metadata_size" .=? ehsMetadataSize
    .: "p"      .=? ehsPort
    .: "reqq"   .=? ehsQueueLength
    .: "v"      .=? ehsVersion
    .: "yourip" .=? (runPut <$> either put put <$> toEither <$> ehsYourIp)
    .: endDict
    where
      toEither (IPv4 v4) = Left v4
      toEither (IPv6 v6) = Right v6

  fromBEncode = fromDict $ ExtendedHandshake
    <$>? "ipv4"
    <*>? "ipv6"
    <*>! "m"
    <*>? "metadata_size"
    <*>? "p"
    <*>? "reqq"
    <*>? "v"
    <*> (opt "yourip" >>= getYourIp)

getYourIp :: Maybe BValue -> BE.Get (Maybe IP)
getYourIp f =
  return $ do
    BString ip <- f
    either (const Nothing) Just $
           case BS.length ip of
             4  -> IPv4 <$> S.decode ip
             16 -> IPv6 <$> S.decode ip
             _ -> fail ""

instance Pretty ExtendedHandshake where
  pretty = PP.text . show

-- | NOTE: Approximated 'stats'.
instance PeerMessage ExtendedHandshake where
  envelop  c = envelop c . EHandshake
  {-# INLINE envelop #-}

  requires _ = Just ExtExtended
  {-# INLINE requires #-}

  stats _ = ByteStats (4 + 1 + 1) 100 {- is it ok? -} 0 -- FIXME
  {-# INLINE stats #-}

-- | Set default values and the specified 'ExtendedCaps'.
nullExtendedHandshake :: ExtendedCaps -> ExtendedHandshake
nullExtendedHandshake caps = ExtendedHandshake
    { ehsIPv4         = Nothing
    , ehsIPv6         = Nothing
    , ehsCaps         = caps
    , ehsMetadataSize = Nothing
    , ehsPort         = Nothing
    , ehsQueueLength  = Just defaultQueueLength
    , ehsVersion      = Just $ T.pack $ render $ pretty libFingerprint
    , ehsYourIp       = Nothing
    }

{-----------------------------------------------------------------------
-- Metadata exchange extension
-----------------------------------------------------------------------}

-- | A peer MUST verify that any piece it sends passes the info-hash
-- verification. i.e. until the peer has the entire metadata, it
-- cannot run SHA-1 to verify that it yields the same hash as the
-- info-hash.
--
data ExtendedMetadata
    -- | This message requests the a specified metadata piece. The
    -- response to this message, from a peer supporting the extension,
    -- is either a 'MetadataReject' or a 'MetadataData' message.
  = MetadataRequest PieceIx

    -- | If sender requested a valid 'PieceIx' and receiver have the
    -- corresponding piece then receiver should respond with this
    -- message.
  | MetadataData
    { -- | A piece of 'Data.Torrent.InfoDict'.
      piece     :: P.Piece BS.ByteString

      -- | This key has the same semantics as the 'ehsMetadataSize' in
      -- the 'ExtendedHandshake' — it is size of the torrent info
      -- dict.
    , totalSize :: Int
    }

    -- | Peers that do not have the entire metadata MUST respond with
    -- a reject message to any metadata request.
    --
    --   Clients MAY implement flood protection by rejecting request
    --   messages after a certain number of them have been
    --   served. Typically the number of pieces of metadata times a
    --   factor.
  | MetadataReject  PieceIx

    -- | Reserved. By specification we should ignore unknown metadata
    -- messages.
  | MetadataUnknown BValue
    deriving (Show, Eq, Typeable)

-- | Extended metadata message id used in 'msg_type_key'.
type MetadataId = Int

msg_type_key, piece_key, total_size_key :: BKey
msg_type_key   = "msg_type"
piece_key      = "piece"
total_size_key = "total_size"

-- | BEP9 compatible encoding.
instance BEncode ExtendedMetadata where
  toBEncode (MetadataRequest pix) = toDict $
       msg_type_key   .=! (0 :: MetadataId)
    .: piece_key      .=! pix
    .: endDict
  toBEncode (MetadataData (P.Piece pix _) totalSize) = toDict $
       msg_type_key   .=! (1 :: MetadataId)
    .: piece_key      .=! pix
    .: total_size_key .=! totalSize
    .: endDict
  toBEncode (MetadataReject  pix)  = toDict $
       msg_type_key   .=! (2 :: MetadataId)
    .: piece_key      .=! pix
    .: endDict
  toBEncode (MetadataUnknown bval) = bval

  fromBEncode bval = (`fromDict` bval) $ do
    mid <- field $ req msg_type_key
    case mid :: MetadataId of
      0 -> MetadataRequest <$>! piece_key
      1 -> metadataData    <$>! piece_key <*>! total_size_key
      2 -> MetadataReject  <$>! piece_key
      _ -> pure (MetadataUnknown bval)
   where
     metadataData pix s = MetadataData (P.Piece pix BS.empty) s

-- | Piece data bytes are omitted.
instance Pretty ExtendedMetadata where
  pretty (MetadataRequest pix  ) = "Request" <+> PP.int    pix
  pretty (MetadataData    p   t) = "Data"    <+>    pretty p   <+> PP.int t
  pretty (MetadataReject  pix  ) = "Reject"  <+> PP.int    pix
  pretty (MetadataUnknown bval ) = "Unknown" <+> ppBEncode bval

-- | NOTE: Approximated 'stats'.
instance PeerMessage ExtendedMetadata where
  envelop  c = envelop c . EMetadata (remoteMessageId ExtMetadata c)
  {-# INLINE envelop #-}

  requires _ = Just ExtExtended
  {-# INLINE requires #-}

  stats (MetadataRequest _) = ByteStats (4 + 1 + 1) {- ~ -} 25 0
  stats (MetadataData p  _) = ByteStats (4 + 1 + 1) {- ~ -} 41 $
                              BS.length (P.pieceData p)
  stats (MetadataReject  _) = ByteStats (4 + 1 + 1) {- ~ -} 25 0
  stats (MetadataUnknown _) = ByteStats (4 + 1 + 1) {- ? -} 0  0

-- | All 'Piece's in 'MetadataData' messages MUST have size equal to
-- this value. The last trailing piece can be shorter.
metadataPieceSize :: Int
metadataPieceSize = 16 * 1024

isLastPiece :: P.Piece a -> Int -> Bool
isLastPiece P.Piece {..} total = succ pieceIndex == pcnt
  where
    pcnt = q + if r > 0 then 1 else 0
    (q, r) = quotRem total metadataPieceSize

-- TODO we can check if the piece payload bytestring have appropriate
-- length; otherwise serialization MUST fail.
isValidPiece :: P.Piece BL.ByteString -> Int -> Bool
isValidPiece p @ P.Piece {..} total
  | isLastPiece p total = P.pieceSize p <= metadataPieceSize
  |       otherwise     = P.pieceSize p == metadataPieceSize

setMetadataPayload :: BS.ByteString -> ExtendedMetadata -> ExtendedMetadata
setMetadataPayload bs (MetadataData (P.Piece pix _) t) =
  MetadataData (P.Piece pix bs) t
setMetadataPayload _   msg               = msg

getMetadataPayload :: ExtendedMetadata -> Maybe BS.ByteString
getMetadataPayload (MetadataData (P.Piece _ bs) _) = Just bs
getMetadataPayload _                                  = Nothing

-- | Metadata BDict usually contain only 'msg_type_key', 'piece_key'
-- and 'total_size_key' fields so it normally should take less than
-- 100 bytes. This limit is two order of magnitude larger to be
-- permissive to 'MetadataUnknown' messages.
--
--   See 'maxMessageSize' for further explanation.
--
maxMetadataBDictSize :: Int
maxMetadataBDictSize = 16 * 1024

maxMetadataSize :: Int
maxMetadataSize = maxMetadataBDictSize + metadataPieceSize

-- to make MetadataData constructor fields a little bit prettier we
-- cheat here: first we read empty 'pieceData' from bdict, but then we
-- fill that field with the actual piece data — trailing bytes of
-- the message
getMetadata :: Int -> S.Get ExtendedMetadata
getMetadata len
    | len > maxMetadataSize = fail $ parseError "size exceeded limit"
    | otherwise = do
      bs <- getByteString len
      parseRes $ BS.parse BE.parser bs
  where
    parseError reason = "unable to parse metadata message: " ++ reason

    parseRes (BS.Fail _ _ m) = fail $ parseError $ "bdict: " ++ m
    parseRes (BS.Partial _)  = fail $ parseError   "bdict: not enough bytes"
    parseRes (BS.Done piece bvalueBS)
      | BS.length piece > metadataPieceSize
      = fail "infodict piece: size exceeded limit"
      | otherwise = do
        metadata <- either (fail . parseError) pure $ fromBEncode bvalueBS
        return $ setMetadataPayload piece metadata

putMetadata :: ExtendedMetadata -> BL.ByteString
putMetadata msg
  | Just bs <- getMetadataPayload msg = BE.encode msg <> BL.fromStrict bs
  |              otherwise            = BE.encode msg

-- | Allows a requesting peer to send 2 'MetadataRequest's for the
--   each piece.
--
--   See 'Network.BitTorrent.Wire.Options.metadataFactor' for
--   explanation why do we need this limit.
defaultMetadataFactor :: Int
defaultMetadataFactor = 2

-- | Usually torrent size do not exceed 1MB. This value limit torrent
--   /content/ size to about 8TB.
--
--   See 'Network.BitTorrent.Wire.Options.maxInfoDictSize' for
--   explanation why do we need this limit.
defaultMaxInfoDictSize :: Int
defaultMaxInfoDictSize = 10 * 1024 * 1024

{-----------------------------------------------------------------------
--  Extension protocol messages
-----------------------------------------------------------------------}

-- | For more info see <http://www.bittorrent.org/beps/bep_0010.html>
data ExtendedMessage
  = EHandshake ExtendedHandshake
  | EMetadata  ExtendedMessageId ExtendedMetadata
  | EUnknown   ExtendedMessageId BS.ByteString
    deriving (Show, Eq, Typeable)

instance Pretty ExtendedMessage where
  pretty (EHandshake     ehs) = pretty ehs
  pretty (EMetadata  _   msg) = "Metadata" <+> pretty msg
  pretty (EUnknown   mid _  ) = "Unknown"  <+> PP.text (show mid)

instance PeerMessage ExtendedMessage where
  envelop  _ = Extended
  {-# INLINE envelop #-}

  requires _ = Just ExtExtended
  {-# INLINE requires #-}

  stats (EHandshake  hs)  = stats hs
  stats (EMetadata _ msg) = stats msg
  stats (EUnknown  _ msg) = ByteStats (4 + 1 + 1) (BS.length msg) 0

{-----------------------------------------------------------------------
-- The message datatype
-----------------------------------------------------------------------}

type MessageId = Word8

-- | Messages used in communication between peers.
--
--   Note: If some extensions are disabled (not present in extension
--   mask) and client receive message used by the disabled
--   extension then the client MUST close the connection.
--
data Message
    -- | Peers may close the TCP connection if they have not received
    -- any messages for a given period of time, generally 2
    -- minutes. Thus, the KeepAlive message is sent to keep the
    -- connection between two peers alive, if no /other/ message has
    -- been sent in a given period of time.
  = KeepAlive
  | Status    !StatusUpdate -- ^ Messages used to update peer status.
  | Available !Available    -- ^ Messages used to inform availability.
  | Transfer  !Transfer     -- ^ Messages used to transfer 'Block's.

    -- | Peer receiving a handshake indicating the remote peer
    -- supports the 'ExtDHT' should send a 'Port' message. Peers that
    -- receive this message should attempt to ping the node on the
    -- received port and IP address of the remote peer.
  | Port      !PortNumber
  | Fast      !FastMessage
  | Extended  !ExtendedMessage
    deriving (Show, Eq)

instance Default Message where
  def = KeepAlive
  {-# INLINE def #-}

-- | Payload bytes are omitted.
instance Pretty Message where
  pretty (KeepAlive  ) = "Keep alive"
  pretty (Status    m) = "Status" <+> pretty m
  pretty (Available m) = pretty m
  pretty (Transfer  m) = pretty m
  pretty (Port      p) = "Port" <+> int (fromEnum p)
  pretty (Fast      m) = pretty m
  pretty (Extended  m) = pretty m

instance PeerMessage Message where
  envelop _ = id
  {-# INLINE envelop #-}

  requires  KeepAlive    = Nothing
  requires (Status    _) = Nothing
  requires (Available _) = Nothing
  requires (Transfer  _) = Nothing
  requires (Port      _) = Just ExtDHT
  requires (Fast      _) = Just ExtFast
  requires (Extended  _) = Just ExtExtended

  stats  KeepAlive    = ByteStats 4 0 0
  stats (Status    m) = stats m
  stats (Available m) = stats m
  stats (Transfer  m) = stats m
  stats (Port      _) = ByteStats 5 2 0
  stats (Fast      m) = stats m
  stats (Extended  m) = stats m

-- | PORT message.
instance PeerMessage PortNumber where
  envelop  _ = Port
  {-# INLINE envelop #-}

  requires _ = Just ExtDHT
  {-# INLINE requires #-}

-- | How long /this/ peer should wait before dropping connection, in
-- seconds.
defaultKeepAliveTimeout :: Int
defaultKeepAliveTimeout = 2 * 60

-- | How often /this/ peer should send 'KeepAlive' messages, in
-- seconds.
defaultKeepAliveInterval :: Int
defaultKeepAliveInterval = 60

getInt :: S.Get Int
getInt = fromIntegral <$> S.getWord32be
{-# INLINE getInt #-}

putInt :: S.Putter Int
putInt = S.putWord32be . fromIntegral
{-# INLINE putInt #-}

-- | This limit should protect against "out-of-memory" attacks: if a
-- malicious peer have sent a long varlength message then receiver can
-- accumulate too long bytestring in the 'Get'.
--
--   Normal messages should never exceed this limits.
--
--   See also 'maxBitfieldSize', 'maxBlockSize' limits.
--
maxMessageSize :: Int
maxMessageSize = 20 + 1024 * 1024

-- | This also limit max torrent size to:
--
--     max_bitfield_size * piece_ix_per_byte * max_piece_size =
--     2 ^ 20 * 8 * 1MB =
--     8TB
--
maxBitfieldSize :: Int
maxBitfieldSize = 1024 * 1024

getBitfield :: Int -> S.Get Bitfield
getBitfield len
  | len > maxBitfieldSize = fail "BITFIELD message size exceeded limit"
  |       otherwise       = fromBitmap <$> getByteString len

maxBlockSize :: Int
maxBlockSize = 4 * defaultTransferSize

getBlock :: Int -> S.Get (Block BL.ByteString)
getBlock len
  | len > maxBlockSize = fail "BLOCK message size exceeded limit"
  | otherwise = Block <$> getInt <*> getInt
                      <*> getLazyByteString (fromIntegral len)
{-# INLINE getBlock #-}

instance Serialize Message where
  get = do
    len <- getInt

    when (len > maxMessageSize) $ do
      fail "message body size exceeded the limit"

    if len == 0 then return KeepAlive
      else do
        mid <- S.getWord8
        case mid of
          0x00 -> return $ Status (Choking True)
          0x01 -> return $ Status (Choking False)
          0x02 -> return $ Status (Interested True)
          0x03 -> return $ Status (Interested False)
          0x04 -> (Available . Have)     <$> getInt
          0x05 -> (Available . Bitfield) <$> getBitfield (pred len)
          0x06 -> (Transfer  . Request)  <$> S.get
          0x07 -> (Transfer  . Piece)    <$> getBlock (len - 9)
          0x08 -> (Transfer  . Cancel)   <$> S.get
          0x09 -> Port <$> S.get
          0x0D -> (Fast . SuggestPiece) <$> getInt
          0x0E -> return $ Fast HaveAll
          0x0F -> return $ Fast HaveNone
          0x10 -> (Fast . RejectRequest) <$> S.get
          0x11 -> (Fast . AllowedFast)   <$> getInt
          0x14 -> Extended <$> getExtendedMessage (pred len)
          _    -> do
            rm <- S.remaining >>= S.getBytes
            fail $ "unknown message ID: " ++ show mid ++ "\n"
                ++ "remaining available bytes: " ++ show rm

  put  KeepAlive      = putInt 0
  put (Status    msg) = putStatus    msg
  put (Available msg) = putAvailable msg
  put (Transfer  msg) = putTransfer  msg
  put (Port      p  ) = putPort      p
  put (Fast      msg) = putFast      msg
  put (Extended  m )  = putExtendedMessage m

statusUpdateId :: StatusUpdate -> MessageId
statusUpdateId (Choking    choking) = fromIntegral (0 + fromEnum choking)
statusUpdateId (Interested choking) = fromIntegral (2 + fromEnum choking)

putStatus :: Putter StatusUpdate
putStatus su = do
  putInt 1
  putWord8 (statusUpdateId su)

putAvailable :: Putter Available
putAvailable (Have i)      = do
  putInt 5
  putWord8 0x04
  putInt i
putAvailable (Bitfield (toBitmap -> bs)) = do
  putInt $ 1 + fromIntegral (BL.length bs)
  putWord8 0x05
  putLazyByteString bs

putBlock :: Putter (Block BL.ByteString)
putBlock Block {..} = do
  putInt blkPiece
  putInt blkOffset
  putLazyByteString blkData

putTransfer :: Putter Transfer
putTransfer (Request blk) = putInt 13 >> S.putWord8 0x06 >> S.put blk
putTransfer (Piece   blk) = do
  putInt (9 + blockSize blk)
  putWord8 0x07
  putBlock blk
putTransfer (Cancel  blk)  = putInt 13 >> S.putWord8 0x08 >> S.put blk

putPort :: Putter PortNumber
putPort p = do
  putInt 3
  putWord8 0x09
  put p

putFast :: Putter FastMessage
putFast  HaveAll           = putInt 1  >> putWord8 0x0E
putFast  HaveNone          = putInt 1  >> putWord8 0x0F
putFast (SuggestPiece pix) = putInt 5  >> putWord8 0x0D >> putInt pix
putFast (RejectRequest i ) = putInt 13 >> putWord8 0x10 >> put i
putFast (AllowedFast   i ) = putInt 5  >> putWord8 0x11 >> putInt i

maxEHandshakeSize :: Int
maxEHandshakeSize = 16 * 1024

getExtendedHandshake :: Int -> S.Get ExtendedHandshake
getExtendedHandshake messageSize
  | messageSize > maxEHandshakeSize
  = fail "extended handshake size exceeded limit"
  | otherwise = do
    bs <- getByteString messageSize
    either fail pure $ BE.decode bs

maxEUnknownSize :: Int
maxEUnknownSize = 64 * 1024

getExtendedUnknown :: Int -> S.Get BS.ByteString
getExtendedUnknown len
  | len > maxEUnknownSize = fail "unknown extended message size exceeded limit"
  |        otherwise      = getByteString len

getExtendedMessage :: Int -> S.Get ExtendedMessage
getExtendedMessage messageSize = do
  msgId   <- getWord8
  let msgBodySize = messageSize - 1
  case msgId of
    0 -> EHandshake      <$> getExtendedHandshake msgBodySize
    1 -> EMetadata msgId <$> getMetadata          msgBodySize
    _ -> EUnknown  msgId <$> getExtendedUnknown   msgBodySize

-- | By spec.
extendedMessageId :: MessageId
extendedMessageId = 20

putExt :: ExtendedMessageId -> BL.ByteString -> Put
putExt mid lbs = do
  putWord32be $ fromIntegral (1 + 1 + BL.length lbs)
  putWord8 extendedMessageId
  putWord8 mid
  putLazyByteString lbs

-- NOTE: in contrast to getExtendedMessage this function put length
-- and message id too!
putExtendedMessage :: Putter ExtendedMessage
putExtendedMessage (EHandshake hs)     = putExt extHandshakeId $ BE.encode hs
putExtendedMessage (EMetadata mid msg) = putExt mid $ putMetadata msg
putExtendedMessage (EUnknown  mid  bs) = putExt mid $ BL.fromStrict bs
