module Network.BitTorrent.PeerWire.Block
       ( BlockIx(..), Block(..)
       , BlockLIx, PieceLIx
       , defaultBlockSize
       , pieceIx, blockIx
       , blockRange, ixRange, isPiece
       ) where

import Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int

type BlockLIx = Int
type PieceLIx = Int


data BlockIx = BlockIx {
    -- ^ Zero-based piece index.
    ixPiece  :: {-# UNPACK #-} !PieceLIx

    -- ^ Zero-based byte offset within the piece.
  , ixOffset :: {-# UNPACK #-} !Int

    -- ^ Block size starting from offset.
  , ixLength :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)


data Block = Block {
    -- ^ Zero-based piece index.
    blkPiece  :: PieceLIx

    -- ^ Zero-based byte offset within the piece.
  , blkOffset :: Int

    -- ^ Payload.
  , blkData   :: ByteString
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

blockIx :: Block -> BlockIx
blockIx = BlockIx <$> blkPiece <*> blkOffset <*> B.length . blkData

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
