-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
--   This module implements mapping from single continious block space
--   to file storage. Storage can be used in two modes:
--
--     * As in memory storage - in this case we don't touch filesystem.
--
--     * As ordinary mmaped file storage - when we need to store
--       data in the filesystem.
--
--
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Cobit.Storage
       ( Storage

         -- * Construction
       , mapStorage, unmapStorage, withStorage

         -- * Modification
       , getBlk, putBlk
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import Data.List as L
import System.FilePath
import System.Directory

import Data.Torrent
import Network.BitTorrent
import System.IO.MMap.Fixed


data Storage = Storage {
    session :: SwarmSession

    -- | Used to map linear block addresses to disjoint mallocated/mmaped adresses.
  , stPayload :: Fixed

    -- | Used to find out block offsets.
  , stPieceSize :: Int

    -- | Used to verify pieces and set finally mark piece as verified.
  , stHashes :: ByteString
  }

-- TODO doc args
mapStorage :: Int -> Maybe FilePath -> FilePath -> ContentInfo -> IO Storage
mapStorage blkSize statusPath contentPath ci = do
  let content_paths = contentLayout contentPath ci
  mapM_ mkDir (L.map fst content_paths)
  Storage <$> coalesceFiles content_paths
          <*> getAllocator statusPath (pieceCount ci) (blockCount blkSize ci)
          <*> pure (ciPieceLength ci)
          <*> pure (ciPieces ci)
  where
    getAllocator (Just path) = error "getAllocator"
    getAllocator Nothing     = error "getAllocator"

    mkDir path = do
      let dirPath = fst (splitFileName path)
      exist <- doesDirectoryExist dirPath
      unless exist $ do
        createDirectoryIfMissing True dirPath

unmapStorage :: Storage -> IO ()
unmapStorage st = error "unmapStorage"


withStorage :: Int -> Maybe FilePath -> FilePath -> ContentInfo
            -> (Storage -> IO a)
            -> IO a
withStorage blkSize statusPath contentPath t =
  bracket (mapStorage blkSize statusPath contentPath t) unmapStorage

isAvailable :: BlockIx -> Storage -> IO Bool
isAvailable ix Storage {..} = error "isAvailable"


putBlk :: Block -> Storage -> IO ()
putBlk blk @ Block {..}  st @ Storage {..} = do
  available <- isAvailable (blockIx blk) st
  unless available $
    writeBytes (blkInterval stPieceSize blk) (Lazy.fromChunks [blkData]) stPayload

-- TODO
getBlk :: BlockIx -> Storage -> IO (Maybe Block)
getBlk ix @ BlockIx {..}  st @ Storage {..} = do
    available <- isAvailable ix st
    if available
      then Just <$> do
        bs <- readBytes (ixInterval stPieceSize ix) stPayload
        return $ Block ixPiece ixOffset (Lazy.toStrict bs)
      else return Nothing

ixInterval :: Int -> BlockIx -> FixedInterval
ixInterval pieceSize BlockIx {..} =
  interval (ixPiece * pieceSize + ixOffset) ixLength

blkInterval :: Int -> Block -> FixedInterval
blkInterval pieceSize Block {..} =
  interval (blkPiece * pieceSize + blkOffset) (B.length blkData)