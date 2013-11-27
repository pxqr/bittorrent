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
module Network.BitTorrent.Exchange.Protocol
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

         -- * Regular messages
       , Message(..)
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Binary as B
import Data.Binary.Get as B
import Data.Binary.Put as B
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy  as BL
import Data.Default
import Data.Serialize as S
import Network
import Network.Socket.ByteString
import Text.PrettyPrint
import Text.PrettyPrint.Class

import Data.Torrent.Bitfield
import Data.Torrent.Block
import Data.Torrent.InfoHash
import Network.BitTorrent.Extension
import Network.BitTorrent.Core.PeerId
import Network.BitTorrent.Core.PeerAddr ()


getInt :: S.Get Int
getInt = fromIntegral <$> S.getWord32be
{-# INLINE getInt #-}

putInt :: S.Putter Int
putInt = S.putWord32be . fromIntegral
{-# INLINE putInt #-}

getIntB :: B.Get Int
getIntB = fromIntegral <$> B.getWord32be
{-# INLINE getIntB #-}

putIntB :: Int -> B.Put
putIntB = B.putWord32be . fromIntegral
{-# INLINE putIntB #-}

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

-- | Messages used in communication between peers.
--
--   Note: If some extensions are disabled (not present in extension
--   mask) and client receive message used by the disabled
--   extension then the client MUST close the connection.
--
data Message = KeepAlive
               -- TODO data PeerStatusUpdate = Choke | Unchoke | Interested | NotInterested
             | Choke
             | Unchoke
             | Interested
             | NotInterested

               -- | Zero-based index of a piece that has just been
               -- successfully downloaded and verified via the hash.
             | Have     !PieceIx

               -- | The bitfield message may only be sent immediately
               -- after the handshaking sequence is complete, and
               -- before any other message are sent. If client have no
               -- pieces then bitfield need not to be sent.
             | Bitfield !Bitfield

               -- | Request for a particular block. If a client is
               -- requested a block that another peer do not have the
               -- peer might not answer at all.
             | Request  !BlockIx

               -- | Response for a request for a block.
             | Piece    !(Block BL.ByteString)

               -- | Used to cancel block requests. It is typically
               -- used during "End Game".
             | Cancel   !BlockIx

             | Port     !PortNumber

               -- TODO data FastMessage = HaveAll | HaveNone | ...
               -- | BEP 6: Then peer have all pieces it might send the
               --   'HaveAll' message instead of 'Bitfield'
               --   message. Used to save bandwidth.
             | HaveAll

               -- | BEP 6: Then peer have no pieces it might send
               -- 'HaveNone' message intead of 'Bitfield'
               -- message. Used to save bandwidth.
             | HaveNone

               -- | BEP 6: This is an advisory message meaning "you
               -- might like to download this piece." Used to avoid
               -- excessive disk seeks and amount of IO.
             | SuggestPiece !PieceIx

               -- | BEP 6: Notifies a requesting peer that its request
               -- will not be satisfied.
             | RejectRequest !BlockIx

               -- | BEP 6: This is an advisory messsage meaning "if
               -- you ask for this piece, I'll give it to you even if
               -- you're choked." Used to shorten starting phase.
             | AllowedFast !PieceIx
               deriving (Show, Eq)

instance Default Message where
  def = KeepAlive
  {-# INLINE def #-}

-- | Payload bytes are omitted.
instance Pretty Message where
  pretty (Bitfield _)       = "Bitfield"
  pretty (Piece blk)        = "Piece"    <+> pretty blk
  pretty (Cancel i )        = "Cancel"   <+> pretty i
  pretty (SuggestPiece pix) = "Suggest"  <+> int pix
  pretty (RejectRequest i ) = "Reject"   <+> pretty i
  pretty  msg               = text (show msg)

instance Serialize Message where
  get = do
    len <- getInt
--    _   <- lookAhead $ ensure len
    if len == 0 then return KeepAlive
      else do
        mid <- S.getWord8
        case mid of
          0x00 -> return Choke
          0x01 -> return Unchoke
          0x02 -> return Interested
          0x03 -> return NotInterested
          0x04 -> Have     <$> getInt
          0x05 -> (Bitfield . fromBitmap) <$> S.getByteString (pred len)
          0x06 -> Request  <$> S.get
          0x07 -> Piece    <$> getBlock (len - 9)
          0x08 -> Cancel   <$> S.get
          0x09 -> Port <$> S.get
          0x0E -> return HaveAll
          0x0F -> return HaveNone
          0x0D -> SuggestPiece  <$> getInt
          0x10 -> RejectRequest <$> S.get
          0x11 -> AllowedFast   <$> getInt
          _    -> do
            rm <- S.remaining >>= S.getBytes
            fail $ "unknown message ID: " ++ show mid ++ "\n"
                ++ "remaining available bytes: " ++ show rm

    where
      getBlock :: Int -> S.Get (Block BL.ByteString)
      getBlock len = Block <$> getInt <*> getInt <*> S.getLazyByteString (fromIntegral len)
      {-# INLINE getBlock #-}


  put KeepAlive     = putInt 0
  put Choke         = putInt 1  >> S.putWord8 0x00
  put Unchoke       = putInt 1  >> S.putWord8 0x01
  put Interested    = putInt 1  >> S.putWord8 0x02
  put NotInterested = putInt 1  >> S.putWord8 0x03
  put (Have i)      = putInt 5  >> S.putWord8 0x04 >> putInt i
  put (Bitfield bf) = putInt l  >> S.putWord8 0x05 >> S.putLazyByteString b
    where b = toBitmap bf
          l = succ (fromIntegral (BL.length b))
          {-# INLINE l #-}
  put (Request blk) = putInt 13 >> S.putWord8 0x06 >> S.put blk
  put (Piece   blk) = putInt l  >> S.putWord8 0x07 >> putBlock
    where l = 9 + fromIntegral (BL.length (blkData blk))
          {-# INLINE l #-}
          putBlock = do putInt (blkPiece blk)
                        putInt (blkOffset  blk)
                        S.putLazyByteString (blkData blk)
          {-# INLINE putBlock #-}

  put (Cancel  blk)      = putInt 13 >> S.putWord8 0x08 >> S.put blk
  put (Port    p  )      = putInt 3  >> S.putWord8 0x09 >> S.put p
  put  HaveAll           = putInt 1  >> S.putWord8 0x0E
  put  HaveNone          = putInt 1  >> S.putWord8 0x0F
  put (SuggestPiece pix) = putInt 5  >> S.putWord8 0x0D >> putInt pix
  put (RejectRequest i ) = putInt 13 >> S.putWord8 0x10 >> S.put i
  put (AllowedFast   i ) = putInt 5  >> S.putWord8 0x11 >> putInt i

instance Binary Message where
  get = do
    len <- getIntB
--    _   <- lookAhead $ ensure len
    if len == 0 then return KeepAlive
      else do
        mid <- B.getWord8
        case mid of
          0x00 -> return Choke
          0x01 -> return Unchoke
          0x02 -> return Interested
          0x03 -> return NotInterested
          0x04 -> Have     <$> getIntB
          0x05 -> (Bitfield . fromBitmap) <$> B.getByteString (pred len)
          0x06 -> Request  <$> B.get
          0x07 -> Piece    <$>   getBlock (len - 9)
          0x08 -> Cancel   <$> B.get
          0x09 -> (Port . fromIntegral) <$> B.getWord16be
          0x0E -> return HaveAll
          0x0F -> return HaveNone
          0x0D -> SuggestPiece  <$> getIntB
          0x10 -> RejectRequest <$> B.get
          0x11 -> AllowedFast   <$> getIntB
          _    -> fail $ "unknown message ID: " ++ show mid
    where
      getBlock :: Int -> B.Get (Block BL.ByteString)
      getBlock len = Block <$> getIntB <*> getIntB
                           <*> B.getLazyByteString (fromIntegral len)
      {-# INLINE getBlock #-}

  put KeepAlive     = putIntB 0
  put Choke         = putIntB 1  >> B.putWord8 0x00
  put Unchoke       = putIntB 1  >> B.putWord8 0x01
  put Interested    = putIntB 1  >> B.putWord8 0x02
  put NotInterested = putIntB 1  >> B.putWord8 0x03
  put (Have i)      = putIntB 5  >> B.putWord8 0x04 >> putIntB i
  put (Bitfield bf) = putIntB l  >> B.putWord8 0x05 >> B.putLazyByteString b
    where b = toBitmap bf
          l = succ (fromIntegral (BL.length b))
          {-# INLINE l #-}
  put (Request blk) = putIntB 13 >> B.putWord8 0x06 >> B.put blk
  put (Piece   blk) = putIntB l  >> B.putWord8 0x07 >> putBlock
    where l = 9 + fromIntegral (BL.length (blkData blk))
          {-# INLINE l #-}
          putBlock = do putIntB (blkPiece blk)
                        putIntB (blkOffset  blk)
                        B.putLazyByteString (blkData blk)
          {-# INLINE putBlock #-}

  put (Cancel  blk)      = putIntB 13 >> B.putWord8 0x08 >> B.put blk
  put (Port    p  )      = putIntB 3  >> B.putWord8 0x09 >> B.putWord16be (fromIntegral p)
  put  HaveAll           = putIntB 1  >> B.putWord8 0x0E
  put  HaveNone          = putIntB 1  >> B.putWord8 0x0F
  put (SuggestPiece pix) = putIntB 5  >> B.putWord8 0x0D >> putIntB pix
  put (RejectRequest i ) = putIntB 13 >> B.putWord8 0x10 >> B.put i
  put (AllowedFast   i ) = putIntB 5  >> B.putWord8 0x11 >> putIntB i
