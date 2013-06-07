-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This modules provides all necessary machinery to work with
--   bitfields. Bitfields are used to keep track indices of complete
--   pieces either peer have or client have.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Bitfield
       ( Bitfield, PieceCount

         -- * Construction
       , haveAll, haveNone, have
       , adjustSize

         -- * Query
       , Data.Bitfield.null
       , haveCount, totalCount, completeness
       , findMin, findMax

       , Frequency, frequencies, rarest

         -- * Combine
       , union
       , intersection
       , difference

         -- * Serialization
       , fromBitmap, toBitmap

#if  defined (TESTING)
         -- * Debug
       , mkBitfield
#endif
       ) where

import Control.Monad
import Control.Monad.ST
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.IntervalSet (IntSet)
import qualified Data.IntervalSet as S
import qualified  Data.IntervalSet.ByteString as S
import           Data.List (foldl')
import           Data.Monoid
import           Data.Ratio
import Network.BitTorrent.PeerWire.Block


-- | Used to represent max set bound. Min set bound is always set to
-- zero.
type PieceCount = Int

-- TODO cache some operations

-- | Bitfields are represented just as integer sets but with
-- restriction: the each set should be within given interval (or
-- subset of the specified interval). Size is used to specify
-- interval, so bitfield of size 10 might contain only indices in
-- interval [0..9].
--
data Bitfield = Bitfield {
    bfSize :: !PieceCount
  , bfSet  :: !IntSet
  } deriving (Show, Read, Eq)

-- Invariants: all elements of bfSet lie in [0..bfSize - 1];

instance Monoid Bitfield where
  {-# SPECIALIZE instance Monoid Bitfield #-}
  mempty  = haveNone 0
  mappend = union
  mconcat = unions

{-----------------------------------------------------------------------
    Construction
-----------------------------------------------------------------------}

-- | The empty bitfield of the given size.
haveNone :: PieceCount -> Bitfield
haveNone s = Bitfield s S.empty

-- | The full bitfield containing all piece indices for the given size.
haveAll :: PieceCount -> Bitfield
haveAll s = Bitfield s (S.interval 0 (s - 1))

-- | Insert the index in the set ignoring out of range indices.
have :: PieceIx -> Bitfield -> Bitfield
have ix Bitfield {..}
  | 0 <= ix && ix < bfSize = Bitfield bfSize (S.insert ix bfSet)
  |      otherwise         = Bitfield bfSize bfSet

-- | Assign new size to bitfield. FIXME Normally, size should be only
-- decreased, otherwise exception raised.
adjustSize :: PieceCount -> Bitfield -> Bitfield
adjustSize s Bitfield {..} = Bitfield s bfSet

{-----------------------------------------------------------------------
    Query
-----------------------------------------------------------------------}

-- | Test if bitifield have no one index: peer do not have anything.
null :: Bitfield -> Bool
null Bitfield {..} = S.null bfSet

-- | Count of peer have pieces.
haveCount :: Bitfield -> PieceCount
haveCount = S.size . bfSet

-- | Total count of pieces and its indices.
totalCount :: Bitfield -> PieceCount
totalCount = bfSize

-- | Ratio of /have/ piece count to the /total/ piece count.
--
--   > forall bf. 0 <= completeness bf <= 1
--
completeness :: Bitfield -> Ratio PieceCount
completeness b = haveCount b % totalCount b

-- | Find first available piece index.
findMin :: Bitfield -> Maybe PieceIx
findMin Bitfield {..}
  | S.null bfSet = Nothing
  |   otherwise  = Just (S.findMin bfSet)

-- | Find last available piece index.
findMax :: Bitfield -> Maybe PieceIx
findMax Bitfield {..}
  | S.null bfSet = Nothing
  |   otherwise  = Just (S.findMax bfSet)

-- | Frequencies are needed in piece selection startegies which use
-- availability quantity to find out the optimal next piece index to
-- download.
type Frequency = Int

-- | How many times each piece index occur in the given bitfield set.
frequencies :: [Bitfield] -> Vector Frequency
frequencies [] = V.fromList []
frequencies xs = runST $ do
    v <- VM.new size
    VM.set v 0
    forM_ xs $ \ Bitfield {..} -> do
      forM_ (S.toList bfSet) $ \ x -> do
        fr <- VM.read v x
        VM.write v x (succ fr)
    V.unsafeFreeze v
  where
    size = maximum (map bfSize xs)

-- TODO it seems like this operation is veeery slow

-- | Find least available piece index. If no piece available return 'Nothing'.
rarest :: [Bitfield] -> Maybe PieceIx
rarest xs
    | V.null freqMap = Nothing
    |     otherwise  = Just $ fst $ V.ifoldr' minIx (0, freqMap V.! 0) freqMap
  where
    freqMap = frequencies xs

    minIx :: PieceIx -> Frequency -> (PieceIx, Frequency) -> (PieceIx, Frequency)
    minIx ix fr acc@(_, fra)
      | fr < fra && fr > 0 = (ix, fr)
      |     otherwise      = acc


{-----------------------------------------------------------------------
    Combine
-----------------------------------------------------------------------}

-- | Find indices at least one peer have.
union :: Bitfield -> Bitfield -> Bitfield
union a b = Bitfield {
    bfSize = bfSize a `max` bfSize b
  , bfSet  = bfSet a `S.union` bfSet b
  }

-- | Find indices both peers have.
intersection :: Bitfield -> Bitfield -> Bitfield
intersection a b = Bitfield {
    bfSize = bfSize a `min` bfSize b
  , bfSet  = bfSet a `S.intersection` bfSet b
  }

-- | Find indices which have first peer but do not have the second peer.
difference :: Bitfield -> Bitfield -> Bitfield
difference a b = Bitfield {
    bfSize = bfSize a -- FIXME is it reasonable?
  , bfSet  = bfSet a `S.difference` bfSet b
  }

-- |
unions :: [Bitfield] -> Bitfield
unions = foldl' union (haveNone 0)

{-----------------------------------------------------------------------
    Serialization
-----------------------------------------------------------------------}

-- | Unpack 'Bitfield' from tightly packed bit array. Note resulting
-- size might be more than real bitfield size, use 'adjustSize'.
fromBitmap :: ByteString -> Bitfield
fromBitmap bs = Bitfield {
    bfSize = B.length bs * 8
  , bfSet  = S.fromByteString bs
  }
{-# INLINE fromBitmap #-}

-- | Pack a 'Bitfield' to tightly packed bit array.
toBitmap :: Bitfield -> Lazy.ByteString
toBitmap Bitfield {..} = Lazy.fromChunks [intsetBM, alignment]
  where
    byteSize  = bfSize `div` 8 + if bfSize `mod` 8 == 0 then 0 else 1
    alignment = B.replicate (byteSize - B.length intsetBM) 0
    intsetBM  = S.toByteString bfSet

{-----------------------------------------------------------------------
    Debug
-----------------------------------------------------------------------}

-- | For internal use only.
mkBitfield :: PieceCount -> [PieceIx] -> Bitfield
mkBitfield s ixs = Bitfield {
    bfSize = s
  , bfSet  = S.splitGT (-1) $ S.splitLT s $ S.fromList ixs
  }