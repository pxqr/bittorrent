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
       ) where

import Control.Applicative
import Control.Exception
import Data.ByteString.Lazy as BL
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
  | mode        == ReadOnly      = throwIO  StorageIsRO
  | pieceSize p /= pieceLen      = throwIO (InvalidSize (pieceSize p))
  | not (isValidIx pieceIndex s) = throwIO (InvalidIndex pieceIndex)
  | otherwise = writeBytes offset pieceData fileMap
  where
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

-- | TODO examples of use
genPieceInfo :: Storage -> IO PieceInfo
genPieceInfo = undefined

-- | TODO examples of use
getBitfield :: Storage -> PieceInfo -> IO Bitfield
getBitfield = undefined