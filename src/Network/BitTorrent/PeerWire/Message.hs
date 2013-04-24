module Network.BitTorrent.PeerWire.Message
       ( Message(..)
       ) where

import Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Serialize

import Network.BitTorrent.PeerWire.Block


-- | Messages used in communication between peers.
--
--   Note: If some extensions are disabled (not present in extension
--   mask) and client receive message used by the disabled
--   extension then the client MUST close the connection.
--
data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested

               -- | Zero-based index of a piece that has just been
               -- successfully downloaded and verified via the hash.
             | Have     PieceIx

               -- | The bitfield message may only be sent immediately
               -- after the handshaking sequence is complete, and
               -- before any other message are sent. If client have no
               -- pieces then bitfield need not to be sent.
             | Bitfield ByteString

               -- | Request for a particular block. If a client is
               -- requested a block that another peer do not have the
               -- peer might not answer at all.
             | Request  BlockIx

               -- | Response for a request for a block.
             | Piece    Block

               -- | Used to cancel block requests. It is typically
               -- used during "End Game".
             | Cancel   BlockIx

             | Port     Int

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
             | SuggestPiece PieceIx

               -- | BEP 6: Notifies a requesting peer that its request
               -- will not be satisfied.
             | RejectRequest BlockIx

               -- | BEP 6: This is an advisory messsage meaning "if
               -- you ask for this piece, I'll give it to you even if
               -- you're choked." Used to shorten starting phase.
             | AllowedFast PieceIx
               deriving (Show, Eq)


instance Serialize Message where
  get = do
    len <- getInt
    lookAhead $ ensure len
    if len == 0 then return KeepAlive
      else do
        mid <- getWord8
        case mid of
          0x00 -> return Choke
          0x01 -> return Unchoke
          0x02 -> return Interested
          0x03 -> return NotInterested
          0x04 -> Have     <$> getInt
          0x05 -> Bitfield <$> getBytes (pred len)
          0x06 -> Request  <$> get
          0x07 -> Piece    <$> getBlock (len - 9)
          0x08 -> Cancel   <$> get
          0x09 -> (Port . fromIntegral) <$> getWord16be
          0x0E -> return HaveAll
          0x0F -> return HaveNone
          0x0D -> SuggestPiece  <$> getInt
          0x10 -> RejectRequest <$> get
          0x11 -> AllowedFast   <$> getInt
          _    -> fail $ "unknown message ID: " ++ show mid

    where
      getBlock :: Int -> Get Block
      getBlock len = Block <$> getInt <*> getInt <*> getBytes len
      {-# INLINE getBlock #-}


  put KeepAlive     = putInt 0
  put Choke         = putInt 1  >> putWord8 0x00
  put Unchoke       = putInt 1  >> putWord8 0x01
  put Interested    = putInt 1  >> putWord8 0x02
  put NotInterested = putInt 1  >> putWord8 0x03
  put (Have i)      = putInt 5  >> putWord8 0x04 >> putInt i
  put (Bitfield b)  = putInt l  >> putWord8 0x05 >> putByteString b
    where l = succ (B.length b)
          {-# INLINE l #-}
  put (Request blk) = putInt 13 >> putWord8 0x06 >> put blk
  put (Piece   blk) = putInt l  >> putWord8 0x07 >> putBlock
    where l = 9 + B.length (blkData blk)
          {-# INLINE l #-}
          putBlock = do putInt (blkPiece blk)
                        putInt (blkOffset  blk)
                        putByteString (blkData blk)
          {-# INLINE putBlock #-}

  put (Cancel  blk)      = putInt 13 >> putWord8 0x08 >> put blk
  put (Port    p  )      = putInt 3  >> putWord8 0x09 >> putWord16be (fromIntegral p)
  put  HaveAll           = putInt 1  >> putWord8 0x0E
  put  HaveNone          = putInt 1  >> putWord8 0x0F
  put (SuggestPiece pix) = putInt 5  >> putWord8 0x0D >> putInt pix
  put (RejectRequest ix) = putInt 13 >> putWord8 0x10 >> put ix
  put (AllowedFast   ix) = putInt 5  >> putWord8 0x11 >> putInt ix
