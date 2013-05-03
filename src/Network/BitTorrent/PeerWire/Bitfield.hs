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
module Network.BitTorrent.PeerWire.Bitfield
       ( Bitfield(..)

         -- * Construction
       , empty, full
       , fromByteString, toByteString

         -- * Query
       , findMin, findMax
       , union, intersection, difference
       , frequencies

         -- * Serialization
       , getBitfield, putBitfield, bitfieldByteCount
       ) where

import Control.Applicative hiding (empty)
import Data.Array.Unboxed
import Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List as L hiding (union)
import Data.Maybe
import Data.Serialize
import Data.Word

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

fromByteString :: ByteString -> Bitfield
fromByteString = MkBitfield
{-# INLINE fromByteString #-}

toByteString :: Bitfield -> ByteString
toByteString = bfBits
{-# INLINE toByteString #-}

combine :: [ByteString] -> Maybe ByteString
combine []         = Nothing
combine as@(a : _) = return $ foldr andBS empty as
  where
    andBS x acc = B.pack (B.zipWith (.&.) x acc)
    empty = B.replicate (B.length a) 0

frequencies :: [Bitfield] -> UArray PieceIx Int
frequencies = undefined

zipWithBF :: (Word8 -> Word8 -> Word8) -> Bitfield -> Bitfield -> Bitfield
zipWithBF f a b = MkBitfield $ B.pack $ B.zipWith f (bfBits a) (bfBits b)
{-# INLINE zipWithBF #-}

union :: Bitfield -> Bitfield -> Bitfield
union = zipWithBF (.|.)

intersection :: Bitfield -> Bitfield -> Bitfield
intersection = zipWithBF (.&.)

difference :: Bitfield -> Bitfield -> Bitfield
difference = zipWithBF diffWord8
  where
    diffWord8 :: Word8 -> Word8 -> Word8
    diffWord8 a b = a .&. (a `xor` b)
    {-# INLINE diffWord8 #-}
{-# INLINE difference #-}



-- | Get min index of piece that the peer have.
findMin :: Bitfield -> Maybe PieceIx
findMin (MkBitfield b) = do
    byteIx <- B.findIndex (0 /=) b
    bitIx  <- findMinWord8 (B.index b byteIx)
    return $ byteIx * bitSize (undefined :: Word8) + bitIx
  where
    -- TODO: bit tricks
    findMinWord8 :: Word8 -> Maybe Int
    findMinWord8 b = L.find (testBit b) [0..bitSize (undefined :: Word8) - 1]
    {-# INLINE findMinWord8 #-}
{-# INLINE findMin #-}


findMax :: Bitfield -> Maybe PieceIx
findMax (MkBitfield b) = do
    byteIx <- (pred (B.length b) -) <$> B.findIndex (0 /=) (B.reverse b)
    bitIx  <- findMaxWord8 (B.index b byteIx)
    return $ byteIx * bitSize (undefined :: Word8) + bitIx
  where
    -- TODO: bit tricks
    findMaxWord8 :: Word8 -> Maybe Int
    findMaxWord8 b = L.find (testBit b)
                             (reverse [0 :: Int ..
                                            bitSize (undefined :: Word8) - 1])

{-# INLINE findMax #-}


getBitfield :: Int -> Get Bitfield
getBitfield n = MkBitfield <$> getBytes n
{-# INLINE getBitfield #-}

putBitfield :: Bitfield -> Put
putBitfield = putByteString . bfBits
{-# INLINE putBitfield #-}

bitfieldByteCount :: Bitfield -> Int
bitfieldByteCount = B.length . bfBits
{-# INLINE bitfieldByteCount #-}
