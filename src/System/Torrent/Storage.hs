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
       , openStorage, closeStorage, withStorage
       , getCompleteBitfield

         -- * Modification
       , getBlk, putBlk, selBlk

         -- * File interface
       , FD, openFD, closeFD, readFD, readFDAll
       ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans

import Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as Lazy
import Data.List as L
import Text.PrettyPrint
import System.FilePath
import System.Directory
import Foreign.C.Error

import Data.Bitfield as BF
import Data.Torrent
import Network.BitTorrent.Exchange.Protocol
import System.IO.MMap.Fixed as Fixed

-- TODO merge piece validation and Sessions.available into one transaction.
data Storage = Storage {
    -- |
    metainfo  :: !Torrent

    -- | Bitmask of complete and verified _pieces_.
  , complete  :: !(TVar  Bitfield)

    -- | Bitmask of complete _blocks_.
  , blocks    :: !(TVar Bitfield)
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

getCompleteBitfield :: Storage -> STM Bitfield
getCompleteBitfield Storage {..} = readTVar complete

{-----------------------------------------------------------------------
    Construction
-----------------------------------------------------------------------}

-- TODO doc args
openStorage :: Torrent -> FilePath -> Bitfield -> IO Storage
openStorage t @ Torrent {..} contentPath bf = do
    let content_paths = contentLayout contentPath tInfo
    mapM_ (mkDir . fst)  content_paths

    let blockSize = defaultBlockSize `min` ciPieceLength tInfo
    print $ "content length " ++ show (contentLength tInfo)
    Storage t <$> newTVarIO bf
              <*> newTVarIO (haveNone (blockCount blockSize tInfo))
              <*> pure blockSize
              <*> coalesceFiles content_paths
  where
    mkDir path = do
      let dirPath = fst (splitFileName path)
      exist <- doesDirectoryExist dirPath
      unless exist $ do
        createDirectoryIfMissing True dirPath

-- TODO
closeStorage :: Storage -> IO ()
closeStorage st = return ()


withStorage :: Torrent -> FilePath -> Bitfield -> (Storage -> IO a) -> IO a
withStorage se path bf = bracket (openStorage se path bf) closeStorage

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
    coeff     = ciPieceLength (tInfo metainfo)  `div` blockSize

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
    writeBytes (blkInterval (ciPieceLength (tInfo metainfo)) blk)  blkData  payload

    markBlock blk st
    validatePiece blkPiece st

markBlock :: Block -> Storage -> IO ()
markBlock Block {..} Storage {..} = {-# SCC markBlock #-} do
  let piLen = ciPieceLength (tInfo metainfo)
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
  let piLen = ciPieceLength (tInfo metainfo)
  bs <- readBytes (ixInterval piLen ix) payload
  return $ Block ixPiece ixOffset bs

getPiece :: PieceIx -> Storage -> IO ByteString
getPiece pix st @ Storage {..} = {-# SCC getPiece #-} do
  let piLen = ciPieceLength (tInfo metainfo)
  let bix   = BlockIx pix 0 piLen
  let bs    = viewBytes (ixInterval piLen bix) payload
  return $! Lazy.toStrict bs

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
      piece <- getPiece pix st
      if checkPiece (tInfo metainfo) pix piece
        then do
          atomically $ modifyTVar' complete (BF.have pix)
          return True
        else do
          print $ "----------------------------- invalid " ++ show pix
--          resetPiece pix st
          return True

-- | Check each piece in the storage against content info hash.
--
--   Note that this function will block until each the entire storage
--   checked. This may take a long time for a big torrents ­ use fork
--   if needed.
--
validateStorage :: Storage -> IO ()
validateStorage st = undefined -- (`validatePiece` st) [0..pieceCount st]

{-----------------------------------------------------------------------
  POSIX-like file interface
------------------------------------------------------------------------
This is useful for virtual filesystem writers and just for per file
interface.
-----------------------------------------------------------------------}
-- TODO reference counting: storage might be closed before all FDs
-- gets closed!
-- or we can forbid to close storage and use finalizers only?

type Offset = Int
type Size   = Int

newtype FD = FD { fdData :: ByteString }

-- TODO implement blocking and non blocking modes?

-- | This call correspond to open(2) with the following parameters:
--
--     * OpenMode = ReadOnly;
--
--     * OpenFileFlags = O_NONBLOCK.
--
openFD :: FilePath -> Storage -> IO (Either Errno FD)
openFD path Storage {..}
  | Just offset <- fileOffset path (tInfo metainfo)
  , Just bs     <- lookupRegion (fromIntegral offset) payload
  = return $ Right $ FD bs
  | otherwise = return $ Left $ eNOENT

-- | This call correspond to close(2).
closeFD :: FD -> IO ()
closeFD _ = return ()

-- TODO return "is dir" error
-- TODO check if region is not out of bound
-- TODO check if region completely downloaded
-- TODO if not we could return EAGAIN
-- TODO enqueue read to piece manager
-- WARN use BS.copy?
-- | This call correspond to pread(2).
readFD :: FD -> Offset -> Size -> IO (Either Errno ByteString)
readFD FD {..} offset len = do
  let res =  B.take len $ B.drop offset fdData
  return $ Right res

readFDAll :: FD -> IO ByteString
readFDAll FD {..} = return fdData

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
    coeff     = ciPieceLength (tInfo metainfo) `div` blockSize


ixInterval :: Int -> BlockIx -> FixedInterval
ixInterval pieceSize BlockIx {..} =
  Fixed.interval (ixPiece * pieceSize + ixOffset) ixLength

blkInterval :: Int -> Block -> FixedInterval
blkInterval pieceSize Block {..} =
  Fixed.interval (blkPiece * pieceSize + blkOffset)
                 (fromIntegral (Lazy.length blkData))