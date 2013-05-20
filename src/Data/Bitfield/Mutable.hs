-- TODO: update documentation
-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--
--   Set of integers with atomic modification. Internally IntSet
--   represented as array of tightly packed bits.
--
--   Note that:
--
--     * Insertion, deletion are atomic, waitfree and failfree.
--
--     * You can avoid copying in conversion if you don't care about
--       referencial transparency or sure that after conversion bitfields
--       never modified.
--
--
{-# OPTIONS -fno-warn-unused-do-bind #-}
module Data.Bitfield.Mutable
       ( IntSet

         -- * Construction
       , empty, full

         -- * Query
--       , lookup, member, notMember
--       , size
       , maxSize

         -- * Modification
--       , insert, delete

         -- * Conversion
       , fromByteString, toByteString

         -- * Unsafe operations
         -- ** Construction
       , create, releaseIntSet

         -- ** Modification
       , insertUnsafe, deleteUnsafe

         -- ** Query
       , lookupUnsafe

         -- ** Conversion
       , fromByteStringUnsafe, toByteStringUnsafe

       ) where

import Control.Applicative hiding (empty)
import Data.Bits.Atomic
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Foreign


-- | Basically 'BitSet' is a wrapper on the 'ForeignPtr'.
data IntSet = IntSet {
    sBasePtr  :: {-# UNPACK #-} !(ForeignPtr Word8)
  , sOffset   :: {-# UNPACK #-} !Int
  , sByteSize :: {-# UNPACK #-} !Int
  , sMaxSize  :: {-# UNPACK #-} !Int
  } deriving Show


maxSize :: IntSet -> Int
maxSize = sMaxSize


create :: Int -> (Int -> Ptr Word8 -> IO a) -> IO IntSet
create n f = do
  let byteSize = sizeInBytes n
  fptr <- mallocForeignPtrBytes byteSize
  withForeignPtr fptr (f byteSize)
  return (IntSet fptr 0 byteSize n)

-- | Create a 'IntSet' with a given size in /bits/.
empty :: Int -> IO IntSet
empty n = create n $ \bn ptr ->
  B.memset ptr 0 (fromIntegral bn)

full :: Int -> IO IntSet
full n = create n $ \bn ptr ->
  B.memset ptr (error "IntSet.full") (fromIntegral bn)


-- | Should be used to free scarce resources immediately.
--
--   WARNING: After this call 'BitField' should not be used.
--   Also you can avoid using it at all if resource is not too scarce.
--
releaseIntSet :: IntSet -> IO ()
releaseIntSet = finalizeForeignPtr . sBasePtr

-- | Set nth bit in the given BifField to 1.
--
--   UNSAFE: no bound checking.
--
insertUnsafe :: Int -> IntSet -> IO ()
insertUnsafe i s =
  withByte s i $ \ptr -> do
    fetchAndOr ptr (bit (bitLoc i))
    return ()
{-# INLINE insertUnsafe #-}


deleteUnsafe :: Int -> IntSet -> IO ()
deleteUnsafe i s =
  withByte s i $ \ptr -> do
    fetchAndAnd ptr (complement (bit (bitLoc i)))
    return ()
{-# INLINE deleteUnsafe #-}

-- | Get nth bit in the given BitField.
--
--   UNSAFE: no bound checking.
--
lookupUnsafe :: Int -> IntSet -> IO Bool
lookupUnsafe n s = withByte s n $ \ptr -> (`testBit` bitLoc n) <$> peek ptr
{-# INLINE lookupUnsafe #-}

fromByteString :: Int -> ByteString -> IntSet
fromByteString n = fromByteStringUnsafe n . B.copy
{-# INLINE fromByteString #-}

toByteString :: IntSet -> ByteString
toByteString = B.copy . toByteStringUnsafe
{-# INLINE toByteString #-}

-- | Convert a 'BitField' to the 'ByteString' /without/ copying,
--   so we can write it to a socket or a file for exsample.
--
--   WARNING: Note that using the resulting 'ByteString' might (and even should)
--   BREAK REFERENCIAL TRANSPARENCY since we can change bits using 'setBitN'
--   after the conversion. Use this function wisely and if and only if
--   you understand the consequences, otherwise the really BAD THINGS WILL HAPPEN
--   or use safe version instead.
--
toByteStringUnsafe :: IntSet -> ByteString
toByteStringUnsafe = B.fromForeignPtr <$> sBasePtr <*> pure 0 <*> sByteSize


-- | Convert a 'ByteString' to 'BitField' /without/ copying,
--   so we can read it from a file or a socket.
--
--   WARNING: Please see 'toByteString' doc, the same apply to this function.
--
fromByteStringUnsafe :: Int -> ByteString -> IntSet
fromByteStringUnsafe n (B.PS fptr a b) = IntSet fptr a b n

baseSize :: (Bits a, Integral a) =>
            a -- ^ Base, should be power of two.
         -> a -- ^ Size.
         -> a -- ^ Size in base.
baseSize base n = (n `div` base) + fromIntegral (fromEnum ((n .&. 0x7) > 0))
{-# SPECIALIZE baseSize :: Int -> Int -> Int #-}
{-# SPECIALIZE baseSize :: Word64 -> Word64 -> Word64 #-}

-------------------------------- internal --------------------------------------
sizeInBytes :: Int -- ^ Length in bits.
            -> Int -- ^ Length in bytes aligned by size of word.
sizeInBytes = baseSize 8
{-# INLINE sizeInBytes #-}

-- TODO: see if shifts and bitwise ands are faster
-- and make portable version if not
byteLoc :: Int -> Int
byteLoc i = i `div` 8 * sizeOf (error "byteLoc" :: Word8)
{-# INLINE bitLoc #-}

bitLoc :: Int -> Int
bitLoc i = i `mod` 8 * sizeOf (error "bitLoc" :: Word8)
{-# INLINE byteLoc #-}

withByte :: IntSet -> Int -> (Ptr Word8 -> IO a) -> IO a
withByte s n action = do
  let offset = sOffset s + byteLoc n
  withForeignPtr (sBasePtr s) $ \ptr ->
    action (ptr `advancePtr` offset)
{-# INLINE withByte #-}