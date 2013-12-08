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
       , Handshake(..)
       , defaultHandshake
       , defaultBTProtocol
       , handshakeSize
       , handshakeMaxSize

         -- * Messages
       , Message        (..)
       , PeerMessage    (..)
       , defaultKeepAliveInterval

         -- ** Core messages
       , StatusUpdate   (..)
       , Available      (..)
       , Transfer       (..)

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
       ) where

import Control.Applicative
import Control.Arrow ((&&&), (***))
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
import Network
import Network.Socket hiding (KeepAlive)
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class

import Data.Torrent.Bitfield
import Data.Torrent.InfoHash
import qualified Data.Torrent.Piece as Data
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

-- | Monoid under intersection.
instance Monoid Caps where
  mempty  = Caps (-1)
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

-- | Handshake message is used to exchange all information necessary
-- to establish connection between peers.
--
data Handshake = Handshake {
    -- | Identifier of the protocol. This is usually equal to defaultProtocol
    hsProtocol    :: BS.ByteString

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
    S.putWord8 (fromIntegral (BS.length hsProtocol))
    S.putByteString hsProtocol
    S.put hsReserved
    S.put hsInfoHash
    S.put hsPeerId

  get = do
    len  <- S.getWord8
    Handshake <$> S.getBytes (fromIntegral len)
              <*> S.get
              <*> S.get
              <*> S.get

-- | Show handshake protocol string, caps and fingerprint.
instance Pretty Handshake where
  pretty Handshake {..}
    = text (BC.unpack hsProtocol) $$
      pretty hsReserved           $$
      pretty (fingerprint hsPeerId)

-- | Get handshake message size in bytes from the length of protocol
-- string.
handshakeSize :: Word8 -> Int
handshakeSize n = 1 + fromIntegral n + 8 + 20 + 20

-- | Maximum size of handshake message in bytes.
handshakeMaxSize :: Int
handshakeMaxSize = handshakeSize maxBound

-- | Default protocol string "BitTorrent protocol" as is.
defaultBTProtocol :: BS.ByteString
defaultBTProtocol = "BitTorrent protocol"

-- | Handshake with default protocol string and reserved bitmask.
defaultHandshake :: InfoHash -> PeerId -> Handshake
defaultHandshake = Handshake defaultBTProtocol def

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

-- | BITFIELD message.
instance PeerMessage Bitfield where
  envelop c = envelop c . Bitfield
  {-# INLINE envelop #-}

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

-- | REQUEST message.
instance PeerMessage BlockIx where
  envelop c = envelop c . Request
  {-# INLINE envelop #-}

-- | PIECE message.
instance PeerMessage (Block BL.ByteString) where
  envelop c = envelop c . Piece
  {-# INLINE envelop #-}

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

    -- | Notifies a requesting peer that its request will not be satisfied.
  | RejectRequest !BlockIx

    -- | This is an advisory messsage meaning "if you ask for this
    -- piece, I'll give it to you even if you're choked." Used to
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
--     * The 'mempty' caps include all known extensions;
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
  |           otherwise            = caps
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
    -- be added if ExtMetadata is enabled in current session /and/
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

--    -- |
--  , yourip  :: Maybe (Either HostAddress HostAddress6)
  } deriving (Show, Eq, Typeable)

extHandshakeId :: ExtendedMessageId
extHandshakeId = 0

-- | Default 'Request' queue size.
defaultQueueLength :: Int
defaultQueueLength = 0

-- | All fields are empty.
instance Default ExtendedHandshake where
  def = ExtendedHandshake def def def def def def def

instance BEncode ExtendedHandshake where
  toBEncode ExtendedHandshake {..} = toDict $
       "ipv4"   .=? ehsIPv4 -- FIXME invalid encoding
    .: "ipv6"   .=? ehsIPv6 -- FIXME invalid encoding
    .: "m"      .=! ehsCaps
    .: "metadata_size" .=? ehsMetadataSize
    .: "p"      .=? ehsPort
    .: "reqq"   .=? ehsQueueLength
    .: "v"      .=? ehsVersion
--    .: "yourip" .=? yourip
    .: endDict

  fromBEncode = fromDict $ ExtendedHandshake
    <$>? "ipv4"
    <*>? "ipv6"
    <*>! "m"
    <*>? "metadata_size"
    <*>? "p"
    <*>? "reqq"
    <*>? "v"
--    <*>? "yourip"

instance Pretty ExtendedHandshake where
  pretty = PP.text . show

instance PeerMessage ExtendedHandshake where
  envelop  c = envelop c . EHandshake
  {-# INLINE envelop #-}

  requires _ = Just ExtExtended
  {-# INLINE requires #-}

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
      piece     :: Data.Piece BS.ByteString

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

    -- | Reserved.
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
  toBEncode (MetadataData (Data.Piece pix _) totalSize) = toDict $
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
     metadataData pix s = MetadataData (Data.Piece pix BS.empty) s

-- | Piece data bytes are omitted.
instance Pretty ExtendedMetadata where
  pretty (MetadataRequest pix  ) = "Request" <+> PP.int    pix
  pretty (MetadataData    pi  t) = "Data"    <+>    pretty pi  <+> PP.int t
  pretty (MetadataReject  pix  ) = "Reject"  <+> PP.int    pix
  pretty (MetadataUnknown bval ) = "Unknown" <+> ppBEncode bval

instance PeerMessage ExtendedMetadata where
  envelop  c = envelop c . EMetadata (remoteMessageId ExtMetadata c)
  {-# INLINE envelop #-}

  requires _ = Just ExtExtended
  {-# INLINE requires #-}

metadataPieceSize :: Int
metadataPieceSize = 16 * 1024

-- TODO we can check if the piece payload bytestring have appropriate
-- length; otherwise serialization MUST fail.
isLastMetadata :: ExtendedMetadata -> Bool
isLastMetadata = undefined -- FIXME

checkPiece :: ExtendedMetadata -> Bool
checkPiece = undefined -- FIXME

setMetadataPayload :: BS.ByteString -> ExtendedMetadata -> ExtendedMetadata
setMetadataPayload bs (MetadataData (Data.Piece pix _) t) =
  MetadataData (Data.Piece pix bs) t
setMetadataPayload _   msg               = msg

getMetadataPayload :: ExtendedMetadata -> Maybe BS.ByteString
getMetadataPayload (MetadataData (Data.Piece _ bs) _) = Just bs
getMetadataPayload _                                  = Nothing

-- to make MetadataData constructor fields a little bit prettier we
-- cheat here: first we read empty 'pieceData' from bdict, but then we
-- fill that field with the actual piece data — trailing bytes of
-- the message
getMetadata :: Int -> S.Get ExtendedMetadata
getMetadata len = do
  bs <- getByteString len
  case BS.parse BE.parser bs of
    BS.Fail _ _ _ -> fail "unable to parse metadata bdict: possible corrupted"
    BS.Partial c  -> fail "unable to parse metadata bdict: not enough bytes"
    BS.Done piece bvalueBS -> do
      let msg = "metadata dictionary is invalid"
      metadata <- either (fail msg) pure $ fromBEncode bvalueBS
      pure $ setMetadataPayload piece metadata

putMetadata :: ExtendedMetadata -> BL.ByteString
putMetadata msg
  | Just bs <- getMetadataPayload msg = BE.encode msg <> BL.fromStrict bs
  |              otherwise            = BE.encode msg

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

-- | PORT message.
instance PeerMessage PortNumber where
  envelop  _ = Port
  {-# INLINE envelop #-}

  requires _ = Just ExtDHT
  {-# INLINE requires #-}

-- | In seconds.
defaultKeepAliveInterval :: Int
defaultKeepAliveInterval = 2 * 60

getInt :: S.Get Int
getInt = fromIntegral <$> S.getWord32be
{-# INLINE getInt #-}

putInt :: S.Putter Int
putInt = S.putWord32be . fromIntegral
{-# INLINE putInt #-}

instance Serialize Message where
  get = do
    len <- getInt
    if len == 0 then return KeepAlive
      else do
        mid <- S.getWord8
        case mid of
          0x00 -> return $ Status (Choking True)
          0x01 -> return $ Status (Choking False)
          0x02 -> return $ Status (Interested True)
          0x03 -> return $ Status (Interested False)
          0x04 -> (Available . Have)    <$> getInt
          0x05 -> (Available . Bitfield . fromBitmap)
                                      <$> S.getByteString (pred len)
          0x06 -> (Transfer . Request) <$> S.get
          0x07 -> (Transfer . Piece)   <$> getBlock (len - 9)
          0x08 -> (Transfer . Cancel)  <$> S.get
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

    where
      getBlock :: Int -> S.Get (Block BL.ByteString)
      getBlock len = Block <$> getInt <*> getInt
                           <*> S.getLazyByteString (fromIntegral len)
      {-# INLINE getBlock #-}

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

getExtendedHandshake :: Int -> S.Get ExtendedHandshake
getExtendedHandshake messageSize = do
  bs <- getByteString messageSize
  either fail pure $ BE.decode bs

getExtendedMessage :: Int -> S.Get ExtendedMessage
getExtendedMessage messageSize = do
  msgId   <- getWord8
  let msgBodySize = messageSize - 1
  case msgId of
    0 -> EHandshake      <$> getExtendedHandshake msgBodySize
    1 -> EMetadata msgId <$> getMetadata          msgBodySize
    _ -> EUnknown  msgId <$> getByteString        msgBodySize

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
putExtendedMessage (EHandshake hs)     = putExt extHandshakeId (BE.encode hs)
putExtendedMessage (EMetadata mid msg) = putExt mid (BE.encode msg)
putExtendedMessage (EUnknown  mid  bs) = putExt mid (BL.fromStrict bs)
