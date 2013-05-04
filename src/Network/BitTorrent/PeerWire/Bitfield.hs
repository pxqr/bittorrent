-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--
--   This module provides Bitfield datatype used to represent sets of
--   piece indexes any peer have. All associated operations should be
--   defined here as well.
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Network.BitTorrent.PeerWire.Bitfield
-- TODO: move to Data.Bitfield
       ( Bitfield(..)

         -- * Construction
       , empty, full
       , toList
       , fromByteString, toByteString

         -- * Query
       , findMin, findMax
       , union, intersection, difference, combine
       , frequencies

         -- * Serialization
       , getBitfield, putBitfield, bitfieldByteCount


       , aligned, alignLow, alignedZip
       ) where

import Control.Applicative hiding (empty)
import Data.Array.Unboxed
import Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.List as L hiding (union)
import Data.Maybe
import Data.Serialize
import Data.Word

import Foreign

import Network.BitTorrent.PeerWire.Block
import Data.Torrent

newtype Bitfield = MkBitfield {
    bfBits :: ByteString
--  , bfSize :: Int
  } deriving (Show, Eq, Ord)


empty :: Int -> Bitfield
empty n = MkBitfield $ B.replicate (sizeInBase n 8) 0
{-# INLINE empty #-}

full :: Int -> Bitfield
full n = MkBitfield $ B.replicate (sizeInBase n 8)  (complement 0)
{-# INLINE full #-}

toList :: Bitfield -> [Bool]
toList (MkBitfield bs) = concatMap unpkg (B.unpack bs)
  where
    unpkg :: Word8 -> [Bool]
    unpkg byte = L.map (testBit byte) [0..bitSize (undefined :: Word8) - 1]
{-# INLINE toList #-}

fromByteString :: ByteString -> Bitfield
fromByteString = MkBitfield
{-# INLINE fromByteString #-}

toByteString :: Bitfield -> ByteString
toByteString = bfBits
{-# INLINE toByteString #-}

getBitfield :: Int -> Get Bitfield
getBitfield n = MkBitfield <$> getBytes n
{-# INLINE getBitfield #-}

putBitfield :: Bitfield -> Put
putBitfield = putByteString . bfBits
{-# INLINE putBitfield #-}

bitfieldByteCount :: Bitfield -> Int
bitfieldByteCount = B.length . bfBits
{-# INLINE bitfieldByteCount #-}


align :: Storable a => Ptr a -> (Ptr a, Int)
align p = tie (alignPtr p) undefined
  where
    tie :: Storable a => (Int -> Ptr a) -> a -> (Ptr a, Int)
    tie f a = (f (alignment a), (alignment a))

alignLow :: Ptr Word8 -> Ptr Word
alignLow ptr =
  let alg  = alignment (undefined :: Word)
      aptr = alignPtr (castPtr ptr) alg :: Ptr Word
  in
    if ptr == castPtr aptr
    then aptr
    else castPtr ((castPtr aptr :: Ptr Word8) `advancePtr` negate alg)

isAlignedBy :: Storable a => Ptr a -> Int -> Bool
isAlignedBy ptr alg = alignPtr ptr alg == ptr

type Mem a    = (Ptr a, Int)

aligned :: Storable a => Mem Word8 -> (Mem Word8, Mem a, Mem Word8)
aligned (ptr, len) =
  let lowPtr  = ptr
      lowLen  = midPtr `minusPtr` ptr
      midOff  = lowLen
      (midPtr, alg) = align (castPtr ptr)
      midLen  = alg * div (len - midOff) alg
      midLenA = midLen `div` alg
      hghOff  = midOff + midLen
      hghPtr  = ptr `advancePtr` hghOff
      hghLen  = len - hghOff
   in
     ((lowPtr, lowLen), (midPtr, midLenA), (hghPtr, hghLen))
  where
{-# INLINE aligned #-}

type Mem3 a = (Ptr a, Ptr a, Ptr a, Int)

emptyMem3 :: Mem3 a
emptyMem3 = (nullPtr, nullPtr, nullPtr, 0)

-- assume resulting memory is aligned
alignedZip :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int
              -> (Mem3 Word, Mem3 Word8)
alignedZip aptr bptr cptr size =
  let alg = alignment (undefined :: Word) in
  if (aptr `isAlignedBy` alg) && (bptr `isAlignedBy` alg)
  then
    let asize = alignLow (nullPtr `plusPtr` size) `minusPtr` nullPtr
    in
       ( (castPtr aptr, castPtr bptr, castPtr cptr, asize `div` alg)
       , ( aptr `advancePtr` asize
         , bptr `advancePtr` asize
         , cptr `advancePtr` asize
         , (size - asize)
         )
       )
  else (emptyMem3, (aptr, bptr, cptr, size))

-- force specialization
zipWithBS :: (Word -> Word -> Word)
          -> (Word8 -> Word8 -> Word8)
          -> ByteString -> ByteString -> ByteString
zipWithBS f g a b =
    let (afptr, aoff, asize) = B.toForeignPtr a
        (bfptr, boff, bsize) = B.toForeignPtr b
        size                = min asize bsize in
    B.unsafeCreate size $ \rptr -> do
      withForeignPtr afptr $ \_aptr -> do
        withForeignPtr bfptr $ \_bptr -> do
          let aptr = _aptr `advancePtr` aoff
          let bptr = _bptr `advancePtr` boff

          let (mid, hgh) = alignedZip aptr bptr rptr size
          zipWords mid
          zipBytes hgh
  where
    zipBytes :: (Ptr Word8, Ptr Word8, Ptr Word8, Int) -> IO ()
    zipBytes (aptr, bptr, rptr, n) = go 0
      where
        go :: Int -> IO ()
        go i |   i < n   = do -- TODO unfold
               av <- peekElemOff aptr i
               bv <- peekElemOff bptr i
               pokeElemOff rptr i (g av bv)
               go (succ i)
             | otherwise = return ()

    zipWords :: (Ptr Word, Ptr Word, Ptr Word, Int) -> IO ()
    zipWords (aptr, bptr, rptr, n) = go 0
      where
        go :: Int -> IO ()
        go i |   i < n   = do -- TODO unfold
               av <- peekElemOff aptr i
               bv <- peekElemOff bptr i
               pokeElemOff rptr i (f av bv)
               go (succ i)
             | otherwise = return ()



zipWithBF :: (forall a. Bits a => a -> a -> a) -> Bitfield -> Bitfield -> Bitfield
zipWithBF f a b = MkBitfield $ zipWithBS f f (bfBits a) (bfBits b)
{-# INLINE zipWithBF #-}

findSet :: ByteString -> Maybe Int
findSet b =
    let (fptr, off, len) = B.toForeignPtr b in
    B.inlinePerformIO $ withForeignPtr fptr $ \_ptr -> do
      let ptr = _ptr `advancePtr` off

      let (low, mid, hgh) = aligned (ptr, len)
      let lowOff = fst low `minusPtr` ptr
      let midOff = fst mid `minusPtr` ptr
      let hghOff = fst hgh `minusPtr` ptr

      let resL = (lowOff +) <$> goFind  low
      let resM = (midOff +) <$> goFind (mid :: Mem Word) -- tune size here
                                       --  TODO: with Word8
                                       -- bytestring findIndex works 2
                                       -- times faster.
      let resH = (hghOff +) <$> goFind  hgh

      let res  = resL <|> resM <|> resH

      -- computation of res should not escape withForeignPtr
      case res of
        Nothing -> return ()
        Just _  -> return ()

      return res

  where
    goFind :: (Storable a, Eq a, Num a) => Mem a -> Maybe Int
    goFind (ptr, n) = go 0
      where
        go :: Int -> Maybe Int
        go i |   i < n   =
               let v = B.inlinePerformIO (peekElemOff ptr i) in
               if v /= 0
                 then Just i
                 else go (succ i)
             | otherwise = Nothing



union :: Bitfield -> Bitfield -> Bitfield
union = zipWithBF (.|.)
{-# INLINE union #-}

intersection :: Bitfield -> Bitfield -> Bitfield
intersection = zipWithBF (.&.)
{-# INLINE intersection #-}

difference :: Bitfield -> Bitfield -> Bitfield
difference = zipWithBF diffWord8
  where
    diffWord8 :: Bits a => a -> a -> a
    diffWord8 a b = a .&. (a `xor` b)
    {-# INLINE diffWord8 #-}
{-# INLINE difference #-}

combine :: [Bitfield] -> Maybe Bitfield
combine [] = Nothing
combine as = return $ foldr1 intersection as

-- | Get min index of piece that the peer have.
findMin :: Bitfield -> Maybe PieceIx
findMin (MkBitfield b) = do
    byteIx <- findSet b
    bitIx  <- findMinWord8 (B.index b byteIx)
    return $ byteIx * bitSize (undefined :: Word8) + bitIx
  where
    -- TODO: bit tricks
    findMinWord8 :: Word8 -> Maybe Int
    findMinWord8 byte = L.find (testBit byte) [0..bitSize (undefined :: Word8) - 1]
    {-# INLINE findMinWord8 #-}
{-# INLINE findMin #-}


findMax :: Bitfield -> Maybe PieceIx
findMax (MkBitfield b) = do
    -- TODO avoid reverse
    byteIx <- (pred (B.length b) -) <$> findSet (B.reverse b)
    bitIx  <- findMaxWord8 (B.index b byteIx)
    return $ byteIx * bitSize (undefined :: Word8) + bitIx
  where
    -- TODO: bit tricks
    findMaxWord8 :: Word8 -> Maybe Int
    findMaxWord8 byte = L.find (testBit byte)
                             (reverse [0 :: Int ..
                                            bitSize (undefined :: Word8) - 1])

{-# INLINE findMax #-}

frequencies :: [Bitfield] -> [Int]
frequencies xs = foldr1 (zipWith (+)) $ map (map fromEnum . toList) xs
