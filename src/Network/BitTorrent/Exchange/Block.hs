-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Blocks are used to transfer pieces.
--
{-# LANGUAGE BangPatterns               #-}
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
       , leadingBlock

         -- * Block bucket
       , Bucket

         -- ** Query
       , Network.BitTorrent.Exchange.Block.null
       , Network.BitTorrent.Exchange.Block.full
       , Network.BitTorrent.Exchange.Block.size
       , Network.BitTorrent.Exchange.Block.spans

         -- ** Construction
       , Network.BitTorrent.Exchange.Block.empty
       , Network.BitTorrent.Exchange.Block.insert
       , Network.BitTorrent.Exchange.Block.insertLazy
       , Network.BitTorrent.Exchange.Block.merge
       , Network.BitTorrent.Exchange.Block.fromList

         -- ** Rendering
       , Network.BitTorrent.Exchange.Block.toPiece

         -- ** Debug
       , Network.BitTorrent.Exchange.Block.valid
       ) where

import Prelude hiding (span)
import Control.Applicative
import Data.Aeson.TH
import Data.ByteString as BS hiding (span)
import Data.ByteString.Lazy as BL hiding (span)
import Data.ByteString.Lazy.Builder as BS
import Data.Default
import Data.Monoid
import Data.List as L hiding (span)
import Data.Serialize as S
import Data.Typeable
import Numeric
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class

import Data.Torrent.JSON
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

$(deriveJSON omitRecordPrefix ''BlockIx)

-- | First block in torrent. Useful for debugging.
instance Default BlockIx where
  def = BlockIx 0 0 defaultTransferSize

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
    ("piece  = " <> int ixPiece  <> ",") <+>
    ("offset = " <> int ixOffset <> ",") <+>
    ("length = " <> int ixLength)

-- | Get location of payload bytes in the torrent content.
blockIxRange :: (Num a, Integral a) => PieceSize -> BlockIx -> (a, a)
blockIxRange piSize BlockIx {..} = (offset, offset + len)
  where
    offset = fromIntegral piSize * fromIntegral ixPiece
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
blockRange :: (Num a, Integral a)
           => PieceSize -> Block BL.ByteString -> (a, a)
blockRange piSize = blockIxRange piSize . blockIx
{-# INLINE blockRange #-}

-- | Test if a block can be safely turned into a piece.
isPiece :: PieceSize -> Block BL.ByteString -> Bool
isPiece pieceLen blk @ (Block i offset _) =
     offset == 0 && blockSize blk == pieceLen && i >= 0
{-# INLINE isPiece #-}

-- | First block in the piece.
leadingBlock :: PieceIx -> BlockSize -> BlockIx
leadingBlock pix blockSize = BlockIx
  { ixPiece  = pix
  , ixOffset = 0
  , ixLength = blockSize
  }
{-# INLINE leadingBlock #-}

{-----------------------------------------------------------------------
--  Bucket
-----------------------------------------------------------------------}

type Pos  = Int
type ChunkSize = Int

-- | A sparse set of blocks used to represent an /in progress/ piece.
data Bucket
  = Nil
  | Span {-# UNPACK #-} !ChunkSize          !Bucket
  | Fill {-# UNPACK #-} !ChunkSize !Builder !Bucket

instance Show Bucket where
  showsPrec i  Nil          = showString ""
  showsPrec i (Span s   xs) = showString "Span " <> showInt s
                           <> showString " "     <> showsPrec i xs
  showsPrec i (Fill s _ xs) = showString "Fill " <> showInt s
                           <> showString " "     <> showsPrec i xs

-- | INVARIANT: 'Nil' should appear only after 'Span' of 'Fill'.
nilInvFailed :: a
nilInvFailed = error "Nil: bucket invariant failed"

valid :: Bucket -> Bool
valid = check Nothing
  where
    check Nothing     Nil           = False -- see 'nilInvFailed'
    check (Just _)    _             = True
    check prevIsSpan (Span sz   xs) =
      prevIsSpan /= Just True &&    -- Span n (NotSpan .. ) invariant
      sz > 0 &&                     -- Span is always non-empty
      check (Just True) xs
    check prevIsSpan (Fill sz b xs) =
      prevIsSpan /= Just True &&    -- Fill n (NotFill .. ) invariant
      sz > 0 &&                     -- Fill is always non-empty
      check (Just False) xs

instance Pretty Bucket where
  pretty Nil = nilInvFailed
  pretty bkt = go bkt
    where
      go  Nil           = PP.empty
      go (Span sz   xs) = "Span" <+> PP.int sz <+> go xs
      go (Fill sz b xs) = "Fill" <+> PP.int sz <+> go xs

-- | Smart constructor: use it when some block is /deleted/ from
-- bucket.
span :: ChunkSize -> Bucket -> Bucket
span sz (Span sz'   xs) = Span (sz + sz') xs
span sz  xxs            = Span  sz        xxs
{-# INLINE span #-}

-- | Smart constructor: use it when some block is /inserted/ to
-- bucket.
fill :: ChunkSize -> Builder -> Bucket -> Bucket
fill sz b (Fill sz' b' xs) = Fill (sz + sz') (b <> b') xs
fill sz b  xxs             = Fill  sz         b        xxs
{-# INLINE fill #-}

{-----------------------------------------------------------------------
--  Bucket queries
-----------------------------------------------------------------------}

-- | /O(1)/. Test if this bucket is empty.
null :: Bucket -> Bool
null  Nil         = nilInvFailed
null (Span _ Nil) = True
null  _           = False
{-# INLINE null #-}

-- | /O(1)/. Test if this bucket is complete.
full :: Bucket -> Bool
full  Nil           = nilInvFailed
full (Fill _ _ Nil) = True
full  _             = False
{-# INLINE full #-}

-- | /O(n)/. Total size of the incompleted piece.
size :: Bucket -> PieceSize
size Nil = nilInvFailed
size bkt = go bkt
  where
    go  Nil           = 0
    go (Span sz   xs) = sz + go xs
    go (Fill sz _ xs) = sz + go xs

-- | /O(n)/. List incomplete blocks to download. If some block have
-- size more than the specified 'BlockSize' then block is split into
-- smaller blocks to satisfy given 'BlockSize'. Small (for
-- e.g. trailing) blocks is not ignored, but returned in-order.
spans :: BlockSize -> Bucket -> [(BlockOffset, BlockSize)]
spans expectedSize = go 0
  where
    go _    Nil           = []
    go off (Span sz   xs) = listChunks off sz ++ go (off + sz) xs
    go off (Fill sz _ xs) = go (off + sz) xs

    listChunks off restSize
      | restSize <= 0 = []
      |   otherwise   = (off, blkSize)
                      : listChunks (off + blkSize) (restSize - blkSize)
      where
        blkSize = min expectedSize restSize

{-----------------------------------------------------------------------
--  Bucket contstruction
-----------------------------------------------------------------------}

-- | /O(1)/. A new empty bucket capable to alloof specified size.
empty :: PieceSize -> Bucket
empty sz
  |   sz < 0  = error "empty: Bucket size must be a non-negative value"
  | otherwise = Span sz Nil
{-# INLINE empty #-}

insertSpan :: Pos -> BS.ByteString -> ChunkSize -> Bucket -> Bucket
insertSpan !pos !bs !span_sz !xs =
  let pref_len = pos
      fill_len = span_sz - pos `min` BS.length bs
      suff_len = (span_sz - pos) - fill_len
  in mkSpan pref_len $
     fill   fill_len (byteString (BS.take fill_len bs)) $
     mkSpan suff_len $
     xs
  where
    mkSpan 0  xs = xs
    mkSpan sz xs = Span sz xs

-- | /O(n)/. Insert a strict bytestring at specified position.
--
--   Best case: if blocks are inserted in sequential order, then this
--   operation should take /O(1)/.
--
insert :: Pos -> BS.ByteString -> Bucket -> Bucket
insert _      _  Nil    = nilInvFailed
insert dstPos bs bucket = go 0 bucket
  where
    intersects curPos sz = dstPos >= curPos && dstPos <= curPos + sz

    go _             Nil     = Nil
    go curPos       (Span sz xs)
      | intersects curPos sz = insertSpan (dstPos - curPos) bs sz xs
      |       otherwise      = span sz    (go (curPos + sz) xs)
    go curPos bkt @ (Fill sz br xs)
      | intersects curPos sz = bkt
      |       otherwise      = fill sz br (go (curPos + sz) xs)

fromList :: PieceSize -> [(Pos, BS.ByteString)] -> Bucket
fromList s = L.foldr (uncurry Network.BitTorrent.Exchange.Block.insert)
                     (Network.BitTorrent.Exchange.Block.empty s)

-- TODO zero-copy
insertLazy :: Pos -> BL.ByteString -> Bucket -> Bucket
insertLazy pos bl = Network.BitTorrent.Exchange.Block.insert pos (BL.toStrict bl)

-- | /O(n)/.
merge :: Bucket -> Bucket -> Bucket
merge = error "Bucket.merge: not implemented"

-- | /O(1)/.
toPiece :: Bucket -> Maybe BL.ByteString
toPiece  Nil           = nilInvFailed
toPiece (Fill _ b Nil) = Just (toLazyByteString b)
toPiece  _             = Nothing
