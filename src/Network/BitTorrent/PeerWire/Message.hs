module Network.BitTorrent.PeerWire.Message
       ( Message(..)
       ) where

import Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Serialize

import Network.BitTorrent.PeerWire.Block


-- TODO comment message constructors
data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have     Int
             | Bitfield ByteString
             | Request  BlockIx
             | Piece    Block
             | Cancel   BlockIx
             | Port     Int
               deriving (Show, Eq)


instance Serialize Message where
  get = do
    len <- getInt
    lookAhead $ ensure len
    if len == 0 then return KeepAlive -- FIX check if BS is empty instead of reading len
      else do
        mid <- getWord8
        case mid of
          0 -> return Choke
          1 -> return Unchoke
          2 -> return Interested
          3 -> return NotInterested
          4 -> Have     <$> getInt
          5 -> Bitfield <$> getBytes (pred len)
          6 -> Request  <$> get
          7 -> Piece    <$> getBlock (len - 9)
          8 -> Cancel   <$> get
          9 -> (Port . fromIntegral) <$> getWord16be
          _ -> fail $ "unknown message ID: " ++ show mid

    where
      getBlock :: Int -> Get Block
      getBlock len = Block <$> getInt <*> getInt <*> getBytes len
      {-# INLINE getBlock #-}

  put KeepAlive     = putInt 0
  put Choke         = putInt 1  >> putWord8 0
  put Unchoke       = putInt 1  >> putWord8 1
  put Interested    = putInt 1  >> putWord8 2
  put NotInterested = putInt 1  >> putWord8 3
  put (Have i)      = putInt 5  >> putWord8 4 >> putInt i
  put (Bitfield b)  = putInt l  >> putWord8 5 >> putByteString b
    where l = succ (B.length b)
          {-# INLINE l #-}
  put (Request blk) = putInt 13 >> putWord8 6 >> put blk
  put (Piece   blk) = putInt l  >> putWord8 7 >> putBlock
    where l = 9 + B.length (blkData blk)
          {-# INLINE l #-}
          putBlock = do putInt (blkPiece blk)
                        putInt (blkOffset  blk)
                        putByteString (blkData blk)
          {-# INLINE putBlock #-}

  put (Cancel  blk) = putInt 13 >> putWord8 8 >> put blk
  put (Port    p  ) = putInt 3  >> putWord8 9 >> putWord16be (fromIntegral p)
