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
--
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module System.Torrent.Storage
       ( Storage

         -- * Construction
       , bindTo, unbind, withStorage

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
import Network.BitTorrent.Exchange.Protocol
import Network.BitTorrent.Internal
import System.IO.MMap.Fixed


data Storage = Storage {
    -- |
    session :: !SwarmSession

    -- | Used to map linear block addresses to disjoint mallocated/mmaped adresses.
  , payload :: !Fixed
  }

pieceSize :: Storage -> Int
pieceSize = ciPieceLength . tInfo . torrentMeta . session

-- TODO doc args
bindTo :: SwarmSession -> FilePath -> IO Storage
bindTo se @ SwarmSession {..} contentPath = do
    let content_paths = contentLayout contentPath (tInfo torrentMeta)
    mapM_ mkDir (L.map fst content_paths)
    Storage se <$> coalesceFiles content_paths
  where
    mkDir path = do
      let dirPath = fst (splitFileName path)
      exist <- doesDirectoryExist dirPath
      unless exist $ do
        createDirectoryIfMissing True dirPath

unbind :: Storage -> IO ()
unbind st = error "unmapStorage"


withStorage :: SwarmSession -> FilePath -> (Storage -> IO a) -> IO a
withStorage se path = bracket (se `bindTo` path) unbind


putBlk :: Block -> Storage -> IO ()
putBlk blk @ Block {..}  st @ Storage {..} = do
  writeBytes (blkInterval (pieceSize st) blk) (Lazy.fromChunks [blkData]) payload

-- TODO
getBlk :: BlockIx -> Storage -> IO Block
getBlk ix @ BlockIx {..}  st @ Storage {..} = do
  bs <- readBytes (ixInterval (pieceSize st) ix) payload
  return $ Block ixPiece ixOffset (Lazy.toStrict bs)

ixInterval :: Int -> BlockIx -> FixedInterval
ixInterval pieceSize BlockIx {..} =
  interval (ixPiece * pieceSize + ixOffset) ixLength

blkInterval :: Int -> Block -> FixedInterval
blkInterval pieceSize Block {..} =
  interval (blkPiece * pieceSize + blkOffset) (B.length blkData)