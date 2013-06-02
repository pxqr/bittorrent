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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Bitfield
       ( Bitfield, PieceCount

         -- * Construction
       , empty
       , insert
       , haveAll, haveNone, have

         -- * Query
       , haveCount, totalCount, completeness
       , findMin, findMax
       , frequencies, rarest

         -- * Combine
       , union
       , intersection
       , difference

         -- * Serialization
       , getBitfield, putBitfield
       , bitfieldByteCount

       , -- * Debug
         mkBitfield
       ) where

import Control.Monad
import Control.Monad.ST
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.IntervalSet (IntSet)
import qualified Data.IntervalSet as S
import           Data.List (foldl')
import           Data.Monoid
import           Data.Ratio
import           Data.Serialize
import Network.BitTorrent.PeerWire.Block


type PieceCount = Int

-- TODO cache some operations

-- | Bitfields are represented just as integer sets but with
-- restriction: the each set should be within given interval (or
-- subset of). Size is used to specify interval, so bitfield of size
-- 10 might contain only indices in interval [0..9].
--
data Bitfield = Bitfield {
    bfSize :: !PieceCount
  , bfSet  :: !IntSet
  } deriving (Show, Read, Eq)

-- Invariants: all elements of bfSet lie in [0..bfSize - 1];

instance Monoid Bitfield where
  {-# SPECIALIZE instance Monoid Bitfield #-}
  mempty  = empty 0
  mappend = union
  mconcat = unions

-- TODO documentation
{-----------------------------------------------------------------------
    Construction
-----------------------------------------------------------------------}

empty :: PieceCount -> Bitfield
empty s = Bitfield s S.empty

insert :: PieceIx -> Bitfield -> Bitfield
insert ix Bitfield {..}
  | 0 <= ix && ix < bfSize = Bitfield bfSize (S.insert ix bfSet)
  |      otherwise         = Bitfield bfSize bfSet

haveNone :: PieceCount -> Bitfield
haveNone = empty

haveAll :: PieceCount -> Bitfield
haveAll s = Bitfield s (S.interval 0 (s - 1))

have :: PieceIx -> Bitfield -> Bitfield
have = insert

{-----------------------------------------------------------------------
    Query
-----------------------------------------------------------------------}

haveCount :: Bitfield -> PieceCount
haveCount = S.size . bfSet

totalCount :: Bitfield -> PieceCount
totalCount = bfSize

-- |
--
--   > forall bf. 0 <= completeness bf <= 1
--
completeness :: Bitfield -> Ratio PieceCount
completeness b = haveCount b % totalCount b

findMin :: Bitfield -> Maybe PieceIx
findMin Bitfield {..}
  | S.null bfSet = Nothing
  |   otherwise  = Just (S.findMin bfSet)

findMax :: Bitfield -> Maybe PieceIx
findMax Bitfield {..}
  | S.null bfSet = Nothing
  |   otherwise  = Just (S.findMax bfSet)

type Frequency = Int

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

rarest :: [Bitfield] -> Maybe PieceIx
rarest xs
    | V.null freqMap = Nothing
    |     otherwise  = Just $ fst $ V.ifoldr minIx (0, freqMap V.! 0) freqMap
  where
    freqMap = frequencies xs

    minIx :: PieceIx -> Frequency -> (PieceIx, Frequency) -> (PieceIx, Frequency)
    minIx ix fr acc@(_, fra)
      | fr < fra && fr > 0 = (ix, fr)
      |     otherwise      = acc



{-----------------------------------------------------------------------
    Combine
-----------------------------------------------------------------------}

union :: Bitfield -> Bitfield -> Bitfield
union a b = Bitfield {
    bfSize = bfSize a `max` bfSize b
  , bfSet  = bfSet a `S.union` bfSet b
  }

intersection :: Bitfield -> Bitfield -> Bitfield
intersection a b = Bitfield {
    bfSize = bfSize a `min` bfSize b
  , bfSet  = bfSet a `S.intersection` bfSet b
  }

difference :: Bitfield -> Bitfield -> Bitfield
difference a b = Bitfield {
    bfSize = bfSize a -- FIXME is it more reasonable?
  , bfSet  = bfSet a `S.difference` bfSet b
  }

unions :: [Bitfield] -> Bitfield
unions = foldl' union (empty 0)

{-----------------------------------------------------------------------
    Serialization
-----------------------------------------------------------------------}

getBitfield :: Int -> Get Bitfield
getBitfield = error "getBitfield"

putBitfield :: Bitfield -> Put
putBitfield = error "putBitfield"

bitfieldByteCount :: Bitfield -> Int
bitfieldByteCount = error "bitfieldByteCount"

{-----------------------------------------------------------------------
    Debug
-----------------------------------------------------------------------}

mkBitfield :: PieceCount -> [PieceIx] -> Bitfield
mkBitfield s ixs = Bitfield {
    bfSize = s
  , bfSet  = S.splitLT s $ S.fromList ixs
  }