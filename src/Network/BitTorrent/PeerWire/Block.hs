module Network.BitTorrent.PeerWire.Block
       ( BlockIx(..), Block(..)
       , defaultBlockSize
       , blockRange, ixRange, pieceIx
       , isPiece
       ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int


data BlockIx = BlockIx {
    ixPiece  :: {-# UNPACK #-} !Int -- ^ Zero-based piece index.
  , ixOffset :: {-# UNPACK #-} !Int -- ^ Zero-based byte offset within the piece.
  , ixLength :: {-# UNPACK #-} !Int -- ^ Block size starting from offset.
  } deriving (Show, Eq)

data Block = Block {
    blkPiece  :: Int         -- ^ Zero-based piece index.
  , blkOffset :: Int         -- ^ Zero-based byte offset within the piece.
  , blkData   :: ByteString  -- ^ Payload.
  } deriving (Show, Eq)


-- | Widely used semi-official block size.
defaultBlockSize :: Int
defaultBlockSize = 16 * 1024


isPiece :: Int -> Block -> Bool
isPiece pieceSize (Block i offset bs) =
  offset == 0 && B.length bs == pieceSize && i >= 0
{-# INLINE isPiece #-}

pieceIx :: Int -> Int -> BlockIx
pieceIx i = BlockIx i 0
{-# INLINE pieceIx #-}

blockRange :: (Num a, Integral a) => Int -> Block -> (a, a)
blockRange pieceSize blk = (offset, offset + len)
  where
    offset = fromIntegral pieceSize * fromIntegral (blkPiece blk)
           + fromIntegral (blkOffset blk)
    len    = fromIntegral (B.length (blkData blk))
{-# INLINE blockRange #-}
{-# SPECIALIZE blockRange :: Int -> Block -> (Int64, Int64) #-}

ixRange :: (Num a, Integral a) => Int -> BlockIx -> (a, a)
ixRange pieceSize ix = (offset, offset + len)
  where
    offset = fromIntegral  pieceSize * fromIntegral (ixPiece ix)
           + fromIntegral (ixOffset ix)
    len    = fromIntegral (ixLength ix)
{-# INLINE ixRange #-}
{-# SPECIALIZE ixRange :: Int -> BlockIx -> (Int64, Int64) #-}
