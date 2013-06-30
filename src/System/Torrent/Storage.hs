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
       , ppStorage

         -- * Construction
       , bindTo, unbind, withStorage

         -- * Modification
       , getBlk, putBlk, selBlk
       ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans

import Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import Data.List as L
import Text.PrettyPrint
import System.FilePath
import System.Directory

import Data.Bitfield as BF
import Data.Torrent
import Network.BitTorrent.Exchange.Protocol
import Network.BitTorrent.Internal
import System.IO.MMap.Fixed as Fixed


data Storage = Storage {
    -- |
    session :: !SwarmSession

    -- |
  , blocks  :: !(TVar Bitfield)
    -- TODO use bytestring for fast serialization
    -- because we need to write this bitmap to disc periodically


  , blockSize :: !Int

    -- | Used to map linear block addresses to disjoint
    -- mallocated/mmaped adresses.
  , payload :: !Fixed
  }

ppStorage :: Storage -> IO Doc
ppStorage Storage {..} = pp <$> readTVarIO blocks
  where
    pp bf = int blockSize

{-----------------------------------------------------------------------
    Construction
-----------------------------------------------------------------------}

-- TODO doc args
bindTo :: SwarmSession -> FilePath -> IO Storage
bindTo se @ SwarmSession {..} contentPath = do
    let contentInfo   = tInfo torrentMeta
    let content_paths = contentLayout contentPath contentInfo
    mapM_ mkDir (L.map fst content_paths)

    let pieceLen  = pieceLength se
    let blockSize = min defaultBlockSize pieceLen
    print $ "content length " ++ show (contentLength contentInfo)
    Storage se <$> newTVarIO (haveNone (blockCount blockSize contentInfo))
               <*> pure blockSize
               <*> coalesceFiles content_paths
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

{-----------------------------------------------------------------------
    Modification
-----------------------------------------------------------------------}

-- TODO to avoid races we might need to try Control.Concurrent.yield
-- TODO make block_payload :: Lazy.ByteString

selBlk :: MonadIO m => PieceIx -> Storage -> m [BlockIx]
selBlk pix st @ Storage {..}
  = liftIO $ {-# SCC selBlk #-} atomically $ do
    mask <- pieceMask pix st
    select mask <$> readTVar blocks
  where
    select mask = fmap mkBix . toList . difference mask
    -- TODO clip upper bound of block index
    mkBix ix  = BlockIx pix (blockSize * (ix - offset)) blockSize

    offset    = coeff * pix
    coeff     = pieceLength session `div` blockSize

--
-- TODO make global lock map -- otherwise we might get broken pieces
--
-- imagine the following situation:
--
--   thread1: write
--   thread1: mark
--
-- this let us avoid races as well
--

-- | Write a block to the storage. If block out of range then block is clipped.
--
--
--
putBlk :: MonadIO m => Block -> Storage -> m Bool
putBlk blk @ Block {..}  st @ Storage {..}
  = liftIO $ {-# SCC putBlk #-} do
--  let blkIx = undefined
--  bm <- readTVarIO blocks
--  unless (member blkIx bm) $ do
    writeBytes (blkInterval (pieceLength session) blk)
               (Lazy.fromChunks [blkData])
                payload

    markBlock blk st
    validatePiece blkPiece st

markBlock :: Block -> Storage -> IO ()
markBlock Block {..} Storage {..} = {-# SCC markBlock #-} do
  let piLen = pieceLength session
  let glIx  = (piLen `div` blockSize) * blkPiece + (blkOffset `div` blockSize)
  atomically $ modifyTVar' blocks (have glIx)

-- | Read a block by given block index. If lower or upper bound out of
-- range then index is clipped.
--
--  Do not block.
--
getBlk :: MonadIO m => BlockIx -> Storage -> m Block
getBlk ix @ BlockIx {..}  st @ Storage {..}
  = liftIO $ {-# SCC getBlk #-} do
  -- TODO check if __piece__ is available
  bs <- readBytes (ixInterval (pieceLength session) ix) payload
  return $ Block ixPiece ixOffset (Lazy.toStrict bs)

getPiece :: PieceIx -> Storage -> IO ByteString
getPiece pix st @ Storage {..} = {-# SCC getPiece #-} do
  let pieceLen = pieceLength session
  let bix      = BlockIx pix 0 (pieceLength session)
  bs <- readBytes (ixInterval pieceLen bix) payload
  return (Lazy.toStrict bs)

resetPiece :: PieceIx -> Storage -> IO ()
resetPiece pix st @ Storage {..}
  = {-# SCC resetPiece #-} atomically $ do
  mask <- pieceMask pix st
  modifyTVar' blocks (`difference` mask)

validatePiece :: PieceIx -> Storage -> IO Bool
validatePiece pix st @ Storage {..} = {-# SCC validatePiece #-} do
  downloaded <- atomically $ isDownloaded pix st
  if not downloaded then return False
    else do
      print $ show pix ++ " downloaded"
      piece <- getPiece pix st
      if checkPiece (tInfo (torrentMeta session)) pix piece
        then return True
        else do
          print $ "----------------------------- invalid " ++ show pix
--          resetPiece pix st
          return True

{-----------------------------------------------------------------------
    Internal
-----------------------------------------------------------------------}

isDownloaded :: PieceIx -> Storage -> STM Bool
isDownloaded pix st @ Storage {..} = do
  bf   <- readTVar blocks
  mask <- pieceMask pix st
  return $ intersection mask bf == mask

pieceMask :: PieceIx -> Storage -> STM Bitfield
pieceMask pix Storage {..} =  do
    bf <- readTVar blocks
    return $ BF.interval (totalCount bf) offset (offset + coeff - 1)
  where
    offset    = coeff * pix
    coeff     = pieceLength session `div` blockSize


ixInterval :: Int -> BlockIx -> FixedInterval
ixInterval pieceSize BlockIx {..} =
  Fixed.interval (ixPiece * pieceSize + ixOffset) ixLength

blkInterval :: Int -> Block -> FixedInterval
blkInterval pieceSize Block {..} =
  Fixed.interval (blkPiece * pieceSize + blkOffset) (B.length blkData)