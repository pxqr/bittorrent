-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Blocks are used to transfer pieces.
--
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BitTorrent.Exchange.Block
       ( -- * Block attributes
         BlockOffset
       , BlockCount
       , BlockSize
       , defaultTransferSize

         -- * Block index
       , BlockIx(..)
       , blockIxRange

         -- * Block data
       , Block(..)
       , blockIx
       , blockSize
       , blockRange
       , isPiece
       ) where

import Control.Applicative
import Data.Aeson.TH
import Data.ByteString.Lazy as BL
import Data.Char
import Data.List as L
import Data.Serialize as S
import Data.Typeable
import Text.PrettyPrint
import Text.PrettyPrint.Class

import Data.Torrent.Piece

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
  } deriving (Show, Eq, Typeable)

$(deriveJSON defaultOptions { fieldLabelModifier = (L.map toLower . L.dropWhile isLower) } ''BlockIx)

getInt :: S.Get Int
getInt = fromIntegral <$> S.getWord32be
{-# INLINE getInt #-}

putInt :: S.Putter Int
putInt = S.putWord32be . fromIntegral
{-# INLINE putInt #-}

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

instance Pretty BlockIx where
  pretty BlockIx {..} =
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
  } deriving (Show, Eq, Functor, Typeable)

-- | Payload is ommitted.
instance Pretty (Block BL.ByteString) where
  pretty = pretty . blockIx
  {-# INLINE pretty #-}

-- | Get size of block /payload/ in bytes.
blockSize :: Block BL.ByteString -> BlockSize
blockSize = fromIntegral . BL.length . blkData
{-# INLINE blockSize #-}

-- | Get block index of a block.
blockIx :: Block BL.ByteString -> BlockIx
blockIx = BlockIx <$> blkPiece <*> blkOffset <*> blockSize

-- | Get location of payload bytes in the torrent content.
blockRange :: (Num a, Integral a) => PieceSize -> Block BL.ByteString -> (a, a)
blockRange pieceSize = blockIxRange pieceSize . blockIx
{-# INLINE blockRange #-}

-- | Test if a block can be safely turned into a piece.
isPiece :: PieceSize -> Block BL.ByteString -> Bool
isPiece pieceLen blk @ (Block i offset _) =
     offset == 0 && blockSize blk == pieceLen && i >= 0
{-# INLINE isPiece #-}
