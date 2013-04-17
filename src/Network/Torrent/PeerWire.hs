-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE DoAndIfThenElse #-}
module Network.Torrent.PeerWire
       ( module Network.Torrent.PeerWire.Handshake
       , Message(..), Block(..), BlockIx(..),
       ) where

import Network.Torrent.PeerWire.Handshake

import Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Serialize

data BlockIx = BlockIx {
    ixPiece  :: {-# UNPACK #-} !Int -- ^ Zero-based piece index.
  , ixOffset :: {-# UNPACK #-} !Int -- ^ Zero-based byte offset within the piece.
  , ixLength :: {-# UNPACK #-} !Int -- ^ Block size starting from offset.
  } deriving (Show, Eq)

data Block = Block {
    blkPiece  :: Int -- ^ Zero-based piece index.
  , blkOffset :: Int -- ^ Zero-based byte offset within the piece.
  , blkData   :: ByteString          -- ^ Payload.
  } deriving (Show, Eq)

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

getInt :: Get Int
getInt = fromIntegral <$> getWord32be
{-# INLINE getInt #-}

putInt :: Putter Int
putInt = putWord32be . fromIntegral
{-# INLINE putInt #-}

instance Serialize BlockIx where
  {-# SPECIALIZE instance Serialize BlockIx #-}
  get = BlockIx <$> getInt <*> getInt <*> getInt
  {-# INLINE get #-}

  put ix = do putInt (ixPiece ix)
              putInt (ixOffset ix)
              putInt (ixLength ix)
  {-# INLINE put #-}

instance Serialize Message where
  get = do
    len <- getInt
    lookAhead $ ensure len
    if len == 0 then return KeepAlive
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