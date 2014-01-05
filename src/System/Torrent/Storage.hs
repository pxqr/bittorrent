-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module implements mapping from single continious piece space
--   to file storage. Storage can be used in two modes:
--
--     * As in memory storage - in this case we don't touch filesystem.
--
--     * As ordinary mmaped file storage - when we need to store
--       data in the filesystem.
--
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
module System.Torrent.Storage
       ( -- * Storage
         Storage
       , StorageFailure (..)

         -- * Construction
       , Mode (..)
       , def
       , open
       , close
       , withStorage

         -- * Query
       , genPieceInfo
       , getBitfield

         -- * Modification
       , writePiece
       , readPiece
       , hintRead
       , unsafeReadPiece

         -- * Streaming
       , sourceStorage
       , sinkStorage
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad as M
import Control.Monad.Trans
import Data.ByteString.Lazy as BL
import Data.Conduit as C
import Data.Conduit.Binary as C
import Data.Conduit.List as C
import Data.Typeable

import Data.Torrent.Bitfield as BF
import Data.Torrent.Layout
import Data.Torrent.Piece
import System.Torrent.FileMap as FM


data StorageFailure
    -- | Occurs on a write operation if the storage has been opened
    --   using 'ReadOnly' mode.
  = StorageIsRO

    -- | Piece index is out of bounds.
  | InvalidIndex PieceIx

    -- | Piece size do not match with one passed to the 'open'
    -- function.
  | InvalidSize  PieceSize
    deriving (Show, Eq, Typeable)

instance Exception StorageFailure

-- TODO validation
data Storage = Storage
  { mode     ::                !Mode
  , pieceLen :: {-# UNPACK #-} !PieceSize
  , fileMap  :: {-# UNPACK #-} !FileMap
  }

-- ResourceT ?
open :: Mode -> PieceSize -> FileLayout FileSize -> IO Storage
open mode s l = Storage mode s <$> mmapFiles mode l

close :: Storage -> IO ()
close Storage {..} = unmapFiles fileMap

withStorage :: Mode -> PieceSize -> FileLayout FileSize
            -> (Storage -> IO ()) -> IO ()
withStorage m s l = bracket (open m s l) close

totalPieces :: Storage -> PieceCount
totalPieces Storage {..} = FM.size fileMap `sizeInBase` pieceLen

isValidIx :: PieceIx -> Storage -> Bool
isValidIx i s = 0 <= i && i < totalPieces s

writePiece :: Piece BL.ByteString -> Storage -> IO ()
writePiece p @ Piece {..} s @ Storage {..}
  |       mode == ReadOnly    = throwIO  StorageIsRO
  | isNotValidIx   pieceIndex = throwIO (InvalidIndex pieceIndex)
  | isNotValidSize pieceIndex (pieceSize p)
                              = throwIO (InvalidSize  (pieceSize p))
  |          otherwise        = writeBytes offset pieceData fileMap
  where
    isNotValidSize pix psize
      | succ pix == pcount = psize /= lastPieceLen -- last piece may be shorter
      |      otherwise     = psize /= pieceLen
      where
        lastPieceLen = fromIntegral (FM.size fileMap `rem` fromIntegral pieceLen)
    {-# INLINE isNotValidSize #-}

    isNotValidIx i = i < 0 || i >= pcount
    {-# INLINE isNotValidIx #-}

    pcount = totalPieces s
    offset = fromIntegral pieceIndex * fromIntegral pieceLen

readPiece :: PieceIx -> Storage -> IO (Piece BL.ByteString)
readPiece pix s @ Storage {..}
  | not (isValidIx pix s) = throwIO (InvalidIndex pix)
  | otherwise = Piece pix <$> readBytes offset sz fileMap
  where
    offset = fromIntegral pix * fromIntegral pieceLen
    sz     = fromIntegral pieceLen

-- | Hint about the coming 'readPiece'. Ignores invalid indexes, for e.g.:
--
--   @forall s. hindRead (-1) s == return ()@
--
hintRead :: PieceIx -> Storage -> IO ()
hintRead _pix Storage {..} = return ()

unsafeReadPiece :: PieceIx -> Storage -> IO (Piece BL.ByteString)
unsafeReadPiece pix s @ Storage {..}
  | not (isValidIx pix s) = throwIO (InvalidIndex pix)
  | otherwise = return $ Piece pix (unsafeReadBytes offset sz fileMap)
  where
    offset = fromIntegral pix * fromIntegral pieceLen
    sz     = fromIntegral pieceLen

-- | Stream storage pieces from first to the last.
sourceStorage :: Storage -> Source IO (Piece BL.ByteString)
sourceStorage s = go 0
  where
    go pix
        | pix < totalPieces s = do
          piece <- liftIO $ readPiece pix s
          liftIO $ hintRead (succ pix) s
          yield piece
          go (succ pix)
        |       otherwise     = return ()

-- | Write stream of pieces to the storage. Fail if storage is 'ReadOnly'.
sinkStorage :: Storage -> Sink (Piece BL.ByteString) IO ()
sinkStorage s = do
  awaitForever $ \ piece ->
    liftIO $ writePiece piece s

-- | This function can be used to generate 'InfoDict' from a set of
-- opened files.
genPieceInfo :: Storage -> IO PieceInfo
genPieceInfo s = do
  hashes <- sourceStorage s $= C.map hashPiece $$ C.sinkLbs
  return $ PieceInfo (pieceLen s) (HashList (BL.toStrict hashes))

-- | Verify storage.
--
--   Throws 'InvalidSize' if piece info size do not match with storage
--   piece size.
--
getBitfield :: Storage -> PieceInfo -> IO Bitfield
getBitfield s @ Storage {..} pinfo @ PieceInfo {..}
  | pieceLen /= piPieceLength = throwIO (InvalidSize piPieceLength)
  | otherwise = M.foldM verifyPiece (BF.haveNone total) [0..total - 1]
  where
    total = totalPieces s

    verifyPiece :: Bitfield -> PieceIx -> IO Bitfield
    verifyPiece bf pix = do
      valid <- checkPieceLazy pinfo <$> readPiece pix s
      return $ if valid then BF.insert pix bf else bf
