-- TODO pprint
-- TODO see if this IntervalMap is overkill: Interval dataty have 4 constrs
-- TODO clarify lifetime in docs
-- TODO use madvise
-- TODO unmap selected interval
-- TODO tests
-- TODO benchmarks
-- TODO unmap overlapped regions
-- [A] TODO lazy mapping for 32 bit arch;
--          we need tricky algorithm and a lot of perf tests
-- TODO use memmove in write bytes
-- TODO write elem, write byte, read byte
-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This library provides mechanism to mmap files to fixed address
--   with fine-grained control. Hovewer, instead of using MAP_FIXED we
--   create our own address space upon virtual address space. If you
--   would like you could call this space as "fixed address space".
--
--   This solves a few problems:
--
--     * Page already in use. If you mmap one file at 0..x addresses and
--     want to map second file to x..y addresses using MAP_FIXED you
--     can get in troubles: page might be mapped already. Raw call to
--     mmap will silently unmap x..y addresses and then mmap our second
--     file. So here we have extra unmap we would like to avoid.
--
--     * Page boundaries. If you mmap one file at x..x+1 you could
--     not map next file to say addresses x+1..x+2.
--
--   Internally we make ordinary call to mmap to map a file and then
--   using /interval map/ we map fixed address space to virtual
--   address space. It takes TODO time in TODO cases.
--
--   Basically this library could be used when we need coalesce
--   several files in arbitrary way. We could map at any position as
--   long as offset + size fit in 'Int'.
--
--   For other details see:
--
--   > http://hackage.haskell.org/package/mmap
--   > man mmap
--
{-# LANGUAGE RecordWildCards #-}
module System.IO.MMap.Fixed
       ( -- * Intervals
         FixedOffset, FileOffset, FixedInterval, FileInterval
       , interval, fileInterval

         -- * Construction
       , Fixed, Bytes
       , System.IO.MMap.Fixed.empty, insertTo
       , coalesceFiles

         -- ** Specialized 'insertTo'
       , mmapTo, mallocTo
       , lookupRegion

         -- * Query
       , upperAddr

         -- * Access
       , viewBytes, readBytes, writeBytes
       , readElem, writeElem
       ) where

import Data.ByteString.Lazy as Lazy
import Data.ByteString.Lazy.Internal as Lazy
import Data.ByteString.Internal as B
import Data.List as L
import Data.Int
import Data.IntervalMap.Strict as M
import Data.IntervalMap.Interval
import System.IO.MMap
import Foreign


type FixedOffset = Int
type FileOffset  = Int64
type Size   = Int


type FileInterval  = (FileOffset, Size)
type FixedInterval = Interval FixedOffset


interval :: FixedOffset -> Size -> FixedInterval
interval off s = IntervalCO off (off + fromIntegral (max 0 s))
{-# INLINE interval #-}

fileInterval :: FileOffset -> Size -> FileInterval
fileInterval off s = (off, s)
{-# INLINE fileInterval #-}

intervalSize :: FixedInterval -> Size
intervalSize i = upperBound i - lowerBound i
{-# INLINE intervalSize #-}


type Bytes = (ForeignPtr Word8, Size)

type FixedMap = IntervalMap FixedOffset Bytes

newtype Fixed = Fixed { imap :: FixedMap }

instance Show Fixed where
  show = show . M.toList . imap


mapIM :: (FixedMap -> FixedMap) -> Fixed -> Fixed
mapIM f s = s { imap = f (imap s) }

empty :: Fixed
empty = Fixed M.empty

coalesceFiles :: [(FilePath, Int)] -> IO Fixed
coalesceFiles = go 0 System.IO.MMap.Fixed.empty
  where
    go _      s []       = return s
    go offset s ((path, bsize) : xs) = do
      s' <- mmapTo path (0, bsize) offset s
      go (offset + bsize) s' xs

upperAddr :: Fixed -> FixedOffset
upperAddr = upperBound . fst . findLast . imap

insertTo :: FixedInterval -> Bytes -> Fixed -> Fixed
insertTo fi mm = mapIM (M.insert fi mm)
{-# INLINE insertTo #-}

mmapTo :: FilePath -> FileInterval -> FixedOffset -> Fixed -> IO Fixed
mmapTo path mrange to s = do
  (fptr, offset, fsize) <- mmapFileForeignPtr path ReadWriteEx (Just mrange)

  let fixed  = interval to fsize
  let mmaped = (fptr, offset)

  return $ insertTo fixed mmaped s

mallocTo :: FixedInterval -> Fixed -> IO Fixed
mallocTo fi s = do
  let bsize = intervalSize fi
  fptr <- mallocForeignPtrBytes bsize
  return (insertTo fi (fptr, 0) s)

lookupRegion :: FixedOffset -> Fixed -> Maybe B.ByteString
lookupRegion offset Fixed {..} =
  case intersecting imap $ IntervalCO offset (succ offset) of
    [(i, (fptr, off))] -> let s = upperBound i - lowerBound i
                          in  Just $ fromForeignPtr fptr off (max 0 s)
    _         -> Nothing

-- | Note: this is unsafe operation.
viewBytes :: FixedInterval -> Fixed -> Lazy.ByteString
viewBytes fi s = fromChunks $ L.map mk $ (imap s `intersecting` fi)
  where
   mk (i, (fptr, offset)) =
     let dropB = max 0 (lowerBound fi - lowerBound i)
         dropT = max 0 (upperBound i  - upperBound fi)
         bsize = intervalSize i - (dropT + dropB)
     in fromForeignPtr fptr (offset + dropB) bsize


readBytes :: FixedInterval -> Fixed -> IO Lazy.ByteString
readBytes fi s = let c = Lazy.copy (viewBytes fi s) in mkCopy c >> return c
{-# INLINE readBytes #-}

writeBytes :: FixedInterval -> Lazy.ByteString -> Fixed -> IO ()
writeBytes fi bs s = bscpy (viewBytes fi s) bs
{-# INLINE writeBytes #-}

-- | Note: this operation takes O(log(files count)) time, if possible
-- use readBytes.
readElem :: Storable a => Fixed -> FixedOffset -> IO a
readElem s offset = go undefined
  where
    go :: Storable a => a -> IO a
    go dont_touch = do
      let bsize = sizeOf dont_touch
      let PS fptr off _ = Lazy.toStrict (viewBytes (interval offset bsize) s)
      withForeignPtr fptr $ \ ptr -> peekByteOff ptr off

writeElem :: Storable a => Fixed -> FixedOffset -> a -> IO ()
writeElem s offset x = do
  let bsize = sizeOf x
  let PS fptr off _ = Lazy.toStrict (viewBytes (interval offset bsize) s)
  withForeignPtr fptr $ \ptr -> pokeByteOff ptr off x


mkCopy :: Lazy.ByteString -> IO ()
mkCopy Empty = return ()
mkCopy (Chunk _ x) = mkCopy x

bscpy :: Lazy.ByteString -> Lazy.ByteString -> IO ()
bscpy (PS _ _ 0 `Chunk` dest_rest) src                         = bscpy dest_rest src
bscpy dest                         (PS _ _ 0 `Chunk` src_rest) = bscpy dest      src_rest
bscpy (PS dest_fptr dest_off dest_size `Chunk` dest_rest)
      (PS src_fptr  src_off  src_size  `Chunk` src_rest)
  = do let csize = min dest_size src_size
       withForeignPtr dest_fptr $ \dest_ptr ->
         withForeignPtr src_fptr $ \src_ptr ->
           memcpy (dest_ptr `advancePtr` dest_off)
                  (src_ptr  `advancePtr` src_off)
                  (fromIntegral csize) -- TODO memmove?
       bscpy (PS dest_fptr (dest_off + csize) (dest_size - csize) `Chunk` dest_rest)
             (PS src_fptr  (src_off  + csize) (src_size  - csize) `Chunk` src_rest)
bscpy _ _ = return ()