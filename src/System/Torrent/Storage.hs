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
module System.Torrent.Storage
       ( Storage

         -- * Construction
       , Mode (..)
       , def
       , open
       , close

         -- * Query
       , genPieceInfo
       , getBitfield

         -- * Modification
       , writePiece
       , readPiece
       , unsafeReadPiece
       ) where

import Control.Applicative
import Data.ByteString.Lazy as BL

import Data.Torrent.Bitfield
import Data.Torrent.Layout
import Data.Torrent.Piece
import System.Torrent.FileMap


-- TODO validation
data Storage = Storage
  { pieceLen :: {-# UNPACK #-} !PieceSize
  , fileMap  :: {-# UNPACK #-} !FileMap
  }

-- ResourceT ?
open :: Mode -> PieceSize -> FileLayout FileSize -> IO Storage
open mode s l = Storage s <$> mmapFiles mode l

close :: Storage -> IO ()
close Storage {..} = unmapFiles fileMap

writePiece :: Piece BL.ByteString -> Storage -> IO ()
writePiece Piece {..} Storage {..} = do
  writeBytes (fromIntegral (pieceIndex * pieceLen)) pieceData fileMap

readPiece :: PieceIx -> Storage -> IO (Piece BL.ByteString)
readPiece pix Storage {..} = do
  bs <- readBytes (fromIntegral (pix * pieceLen))
                  (fromIntegral pieceLen) fileMap
  return $ Piece pix bs

unsafeReadPiece :: PieceIx -> Storage -> IO (Piece BL.ByteString)
unsafeReadPiece pix Storage {..} = return $ Piece pix lbs
  where
    lbs = unsafeReadBytes (fromIntegral (pix * pieceLen))
                          (fromIntegral pieceLen) fileMap

-- | TODO examples of use
genPieceInfo :: Storage -> IO PieceInfo
genPieceInfo = undefined

-- | TODO examples of use
getBitfield :: Storage -> PieceInfo -> IO Bitfield
getBitfield = undefined