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
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS  -fno-warn-orphans #-}
module Network.BitTorrent.Exchange.Message
       ( -- * Initial handshake
         Handshake(..)
       , handshake
       , handshakeCaps
       , recvHandshake
       , sendHandshake

         -- ** Defaults
       , defaultHandshake
       , defaultBTProtocol
       , defaultReserved
       , handshakeMaxSize

         -- * Messages
       , Message        (..)
       , StatusUpdate   (..)
       , RegularMessage (..)
       , FastMessage    (..)
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy  as BL
import Data.Default
import Data.Serialize as S
import Data.Word
import Network
import Network.Socket.ByteString
import Text.PrettyPrint
import Text.PrettyPrint.Class

import Data.Torrent.Bitfield
import Data.Torrent.Block
import Data.Torrent.InfoHash
import Network.BitTorrent.Core.PeerId
import Network.BitTorrent.Core.PeerAddr ()
import Network.BitTorrent.Exchange.Extension

{-----------------------------------------------------------------------
    Handshake
-----------------------------------------------------------------------}

-- | Handshake message is used to exchange all information necessary
-- to establish connection between peers.
--
data Handshake = Handshake {
    -- | Identifier of the protocol.
    hsProtocol    :: BS.ByteString

    -- | Reserved bytes used to specify supported BEP's.
  , hsReserved    :: Capabilities

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
  put hs = do
    S.putWord8 (fromIntegral (BS.length (hsProtocol hs)))
    S.putByteString (hsProtocol hs)
    S.putWord64be   (hsReserved hs)
    S.put (hsInfoHash hs)
    S.put (hsPeerId hs)

  get = do
    len  <- S.getWord8
    Handshake <$> S.getBytes (fromIntegral len)
              <*> S.getWord64be
              <*> S.get
              <*> S.get

instance Pretty Handshake where
  pretty Handshake {..}
    = text (BC.unpack hsProtocol) <+> pretty (clientInfo hsPeerId)

-- | Extract capabilities from a peer handshake message.
handshakeCaps :: Handshake -> Capabilities
handshakeCaps = hsReserved


-- | Get handshake message size in bytes from the length of protocol
-- string.
handshakeSize :: Word8 -> Int
handshakeSize n = 1 + fromIntegral n + 8 + 20 + 20

-- | Maximum size of handshake message in bytes.
handshakeMaxSize :: Int
handshakeMaxSize = handshakeSize 255

-- | Default protocol string "BitTorrent protocol" as is.
defaultBTProtocol :: BS.ByteString
defaultBTProtocol = "BitTorrent protocol"

-- | Default reserved word is 0.
defaultReserved :: Word64
defaultReserved = 0

-- | Length of info hash and peer id is unchecked, so it /should/ be
-- equal 20.
defaultHandshake :: InfoHash -> PeerId -> Handshake
defaultHandshake = Handshake defaultBTProtocol defaultReserved

sendHandshake :: Socket -> Handshake -> IO ()
sendHandshake sock hs = sendAll sock (S.encode hs)

recvHandshake :: Socket -> IO Handshake
recvHandshake sock = do
    header <- recv sock 1
    unless (BS.length header == 1) $
      throw $ userError "Unable to receive handshake header."

    let protocolLen = BS.head header
    let restLen     = handshakeSize protocolLen - 1

    body <- recv sock restLen
    let resp = BS.cons protocolLen body
    either (throwIO . userError) return $ S.decode resp

-- | Handshaking with a peer specified by the second argument.
handshake :: Socket -> Handshake -> IO Handshake
handshake sock hs = do
    sendHandshake sock hs
    hs' <- recvHandshake sock
    when (hsInfoHash hs /= hsInfoHash hs') $ do
      throwIO $ userError  "Handshake info hash do not match."
    return hs'

{-----------------------------------------------------------------------
    Regular messages
-----------------------------------------------------------------------}

data StatusUpdate
  = Choke
  | Unchoke
  | Interested
  | NotInterested
    deriving (Show, Eq, Ord, Enum, Bounded)

instance Pretty StatusUpdate where
  pretty = text . show

data RegularMessage =
    -- | Zero-based index of a piece that has just been successfully
    -- downloaded and verified via the hash.
    Have    ! PieceIx

    -- | The bitfield message may only be sent immediately after the
    -- handshaking sequence is complete, and before any other message
    -- are sent. If client have no pieces then bitfield need not to be
    -- sent.
  | Bitfield !Bitfield

    -- | Request for a particular block. If a client is requested a
    -- block that another peer do not have the peer might not answer
    -- at all.
  | Request ! BlockIx

    -- | Response to a request for a block.
  | Piece   !(Block BL.ByteString)

    -- | Used to cancel block requests. It is typically used during
    -- "End Game".
  | Cancel  !BlockIx
    deriving (Show, Eq)

instance Pretty RegularMessage where
  pretty (Have     ix ) = "Have"     <+> int ix
  pretty (Bitfield _  ) = "Bitfield"
  pretty (Request  ix ) = "Request"  <+> pretty ix
  pretty (Piece    blk) = "Piece"    <+> pretty blk
  pretty (Cancel   i  ) = "Cancel"   <+> pretty i

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

-- | Messages used in communication between peers.
--
--   Note: If some extensions are disabled (not present in extension
--   mask) and client receive message used by the disabled
--   extension then the client MUST close the connection.
--
data Message
    -- core
  = KeepAlive
  | Status   !StatusUpdate
  | Regular  !RegularMessage

    -- extensions
  | Port     !PortNumber
  | Fast     !FastMessage
    deriving (Show, Eq)

instance Default Message where
  def = KeepAlive
  {-# INLINE def #-}

-- | Payload bytes are omitted.
instance Pretty Message where
  pretty (KeepAlive  ) = "Keep alive"
  pretty (Status    m) = pretty m
  pretty (Regular   m) = pretty m
  pretty (Port      p) = "Port" <+> int (fromEnum p)
  pretty (Fast      m) = pretty m

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
          0x00 -> return $ Status Choke
          0x01 -> return $ Status Unchoke
          0x02 -> return $ Status Interested
          0x03 -> return $ Status NotInterested
          0x04 -> (Regular . Have)    <$> getInt
          0x05 -> (Regular . Bitfield . fromBitmap)
                                      <$> S.getByteString (pred len)
          0x06 -> (Regular . Request) <$> S.get
          0x07 -> (Regular . Piece)   <$> getBlock (len - 9)
          0x08 -> (Regular . Cancel)  <$> S.get
          0x09 -> Port <$> S.get
          0x0D -> (Fast . SuggestPiece) <$> getInt
          0x0E -> return $ Fast HaveAll
          0x0F -> return $ Fast HaveNone
          0x10 -> (Fast . RejectRequest) <$> S.get
          0x11 -> (Fast . AllowedFast)   <$> getInt
          _    -> do
            rm <- S.remaining >>= S.getBytes
            fail $ "unknown message ID: " ++ show mid ++ "\n"
                ++ "remaining available bytes: " ++ show rm

    where
      getBlock :: Int -> S.Get (Block BL.ByteString)
      getBlock len = Block <$> getInt <*> getInt
                           <*> S.getLazyByteString (fromIntegral len)
      {-# INLINE getBlock #-}

  put  KeepAlive    = putInt 0
  put (Status  msg) = putStatus  msg
  put (Regular msg) = putRegular msg
  put (Port    p  ) = putPort    p
  put (Fast    msg) = putFast    msg

putStatus :: Putter StatusUpdate
putStatus su = putInt 1  >> S.putWord8 (fromIntegral (fromEnum su))

putRegular :: Putter RegularMessage
putRegular (Have i)      = putInt 5  >> S.putWord8 0x04 >> putInt i
putRegular (Bitfield bf) = putInt l  >> S.putWord8 0x05 >> S.putLazyByteString b
  where b = toBitmap bf
        l = succ (fromIntegral (BL.length b))
        {-# INLINE l #-}
putRegular (Request blk) = putInt 13 >> S.putWord8 0x06 >> S.put blk
putRegular (Piece   blk) = putInt l  >> S.putWord8 0x07 >> putBlock
  where l = 9 + fromIntegral (BL.length (blkData blk))
        {-# INLINE l #-}
        putBlock = do putInt (blkPiece blk)
                      putInt (blkOffset  blk)
                      S.putLazyByteString (blkData blk)
        {-# INLINE putBlock #-}
putRegular (Cancel  blk)      = putInt 13 >> S.putWord8 0x08 >> S.put blk

putPort :: Putter PortNumber
putPort p = putInt 3  >> S.putWord8 0x09 >> S.put p

putFast :: Putter FastMessage
putFast  HaveAll           = putInt 1  >> S.putWord8 0x0E
putFast  HaveNone          = putInt 1  >> S.putWord8 0x0F
putFast (SuggestPiece pix) = putInt 5  >> S.putWord8 0x0D >> putInt pix
putFast (RejectRequest i ) = putInt 13 >> S.putWord8 0x10 >> S.put i
putFast (AllowedFast   i ) = putInt 5  >> S.putWord8 0x11 >> putInt i
