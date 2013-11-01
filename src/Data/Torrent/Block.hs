-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Blocks are used to transfer pieces.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Data.Torrent.Block
       ( -- * Piece attributes
         PieceIx
       , PieceSize

         -- * Block attributes
       , BlockOffset
       , BlockCount
       , BlockSize
       , defaultTransferSize

         -- * Block index
       , BlockIx(..)
       , ppBlockIx
       , blockIxRange

         -- * Block data
       , Block(..)
       , ppBlock
       , blockIx
       , blockSize
       , blockRange
       ) where

import Control.Applicative

import Data.Aeson.TH
import qualified Data.ByteString.Lazy as Lazy
import Data.Char
import Data.List as L

import Data.Binary as B
import Data.Binary.Get as B
import Data.Binary.Put as B
import Data.Serialize as S

import Text.PrettyPrint


{-----------------------------------------------------------------------
--  Piece attributes
-----------------------------------------------------------------------}

-- | Zero-based index of piece in torrent content.
type PieceIx     = Int

-- | Size of piece in bytes. Should be a power of 2.
type PieceSize = Int

{-----------------------------------------------------------------------
--  Block attributes
-----------------------------------------------------------------------}

-- | Offset of a block in a piece in bytes. Should be multiple of
-- the choosen block size.
type BlockOffset = Int

-- | Size of a block in bytes. Should be power of 2.
--
--   Normally block size is equal to 'defaultTransferSize'.
--
type BlockSize   = Int

-- | Number of block in a piece of a torrent. Used to distinguish
-- block count from piece count.
type BlockCount  = Int

-- | Widely used semi-official block size. Some clients can ignore if
-- block size of BlockIx in Request message is not equal to this
-- value.
--
defaultTransferSize :: BlockSize
defaultTransferSize = 16 * 1024

{-----------------------------------------------------------------------
    Block Index
-----------------------------------------------------------------------}

-- | BlockIx correspond.
data BlockIx = BlockIx {
    -- | Zero-based piece index.
    ixPiece  :: {-# UNPACK #-} !PieceIx

    -- | Zero-based byte offset within the piece.
  , ixOffset :: {-# UNPACK #-} !BlockOffset

    -- | Block size starting from offset.
  , ixLength :: {-# UNPACK #-} !BlockSize
  } deriving (Show, Eq)

$(deriveJSON (L.map toLower . L.dropWhile isLower) ''BlockIx)

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

instance Serialize BlockIx where
  {-# SPECIALIZE instance Serialize BlockIx #-}
  get = BlockIx <$> getInt
                <*> getInt
                <*> getInt
  {-# INLINE get #-}

  put BlockIx {..} = do
    putInt ixPiece
    putInt ixOffset
    putInt ixLength
  {-# INLINE put #-}

instance Binary BlockIx where
  {-# SPECIALIZE instance Binary BlockIx #-}
  get = BlockIx <$> getIntB
                <*> getIntB
                <*> getIntB
  {-# INLINE get #-}

  put BlockIx {..} = do
    putIntB ixPiece
    putIntB ixOffset
    putIntB ixLength

-- | Format block index in human readable form.
ppBlockIx :: BlockIx -> Doc
ppBlockIx BlockIx {..} =
  "piece  = " <> int ixPiece  <> "," <+>
  "offset = " <> int ixOffset <> "," <+>
  "length = " <> int ixLength

-- | Get location of payload bytes in the torrent content.
blockIxRange :: (Num a, Integral a) => PieceSize -> BlockIx -> (a, a)
blockIxRange pieceSize BlockIx {..} = (offset, offset + len)
  where
    offset = fromIntegral  pieceSize * fromIntegral ixPiece
           + fromIntegral ixOffset
    len    = fromIntegral ixLength
{-# INLINE blockIxRange #-}

{-----------------------------------------------------------------------
    Block
-----------------------------------------------------------------------}

data Block payload = Block {
    -- | Zero-based piece index.
    blkPiece  :: {-# UNPACK #-} !PieceIx

    -- | Zero-based byte offset within the piece.
  , blkOffset :: {-# UNPACK #-} !BlockOffset

    -- | Payload bytes.
  , blkData   :: !payload
  } deriving (Show, Eq)

-- | Format block in human readable form. Payload is ommitted.
ppBlock :: Block Lazy.ByteString -> Doc
ppBlock = ppBlockIx . blockIx
{-# INLINE ppBlock #-}

-- | Get size of block /payload/ in bytes.
blockSize :: Block Lazy.ByteString -> BlockSize
blockSize blk = fromIntegral (Lazy.length (blkData blk))
{-# INLINE blockSize #-}

-- | Get block index of a block.
blockIx :: Block Lazy.ByteString -> BlockIx
blockIx = BlockIx <$> blkPiece <*> blkOffset <*> blockSize

-- | Get location of payload bytes in the torrent content.
blockRange :: (Num a, Integral a) => PieceSize -> Block Lazy.ByteString -> (a, a)
blockRange pieceSize = blockIxRange pieceSize . blockIx
{-# INLINE blockRange #-}
