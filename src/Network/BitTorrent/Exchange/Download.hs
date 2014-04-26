-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--
--
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Network.BitTorrent.Exchange.Download
       ( -- * Downloading
         Download (..)
       , Updates
       , runDownloadUpdates

         -- ** Metadata
         -- $metadata-download
       , MetadataDownload
       , metadataDownload

         -- ** Content
         -- $content-download
       , ContentDownload
       , contentDownload
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad.State
import Data.BEncode as BE
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Default
import Data.List as L
import Data.Maybe
import Data.Map as M
import Data.Tuple

import Data.Torrent as Torrent
import Network.BitTorrent.Address
import Network.BitTorrent.Exchange.Bitfield as BF
import Network.BitTorrent.Exchange.Block    as Block
import Network.BitTorrent.Exchange.Message  as Msg
import System.Torrent.Storage (Storage, writePiece)


{-----------------------------------------------------------------------
--  Class
-----------------------------------------------------------------------}

type Updates s a = StateT s IO a

runDownloadUpdates :: MVar s -> Updates s a -> IO a
runDownloadUpdates var m = modifyMVar var (fmap swap . runStateT m)

class Download s chunk | s -> chunk where
  scheduleBlocks :: Int -> PeerAddr IP -> Bitfield -> Updates s [BlockIx]

  -- |
  scheduleBlock  :: PeerAddr IP -> Bitfield -> Updates s (Maybe BlockIx)
  scheduleBlock addr bf = listToMaybe <$> scheduleBlocks 1 addr bf

  -- | Get number of sent requests to this peer.
  getRequestQueueLength :: PeerAddr IP -> Updates s Int

  -- | Remove all pending block requests to the remote peer. May be used
  -- when:
  --
  --     * a peer closes connection;
  --
  --     * remote peer choked this peer;
  --
  --     * timeout expired.
  --
  resetPending :: PeerAddr IP -> Updates s ()

  -- | MAY write to storage, if a new piece have been completed.
  --
  --  You should check if a returned by peer block is actually have
  -- been requested and in-flight. This is needed to avoid "I send
  -- random corrupted block" attacks.
  pushBlock :: PeerAddr IP -> chunk -> Updates s (Maybe Bool)

{-----------------------------------------------------------------------
--  Metadata download
-----------------------------------------------------------------------}
-- $metadata-download
-- TODO

data MetadataDownload = MetadataDownload
  { _pendingPieces :: [(PeerAddr IP, PieceIx)]
  , _bucket        :: Bucket
  , _topic         :: InfoHash
  }

makeLenses ''MetadataDownload

-- | Create a new scheduler for infodict of the given size.
metadataDownload :: Int -> InfoHash -> MetadataDownload
metadataDownload ps = MetadataDownload [] (Block.empty ps)

instance Default MetadataDownload where
  def = error "instance Default MetadataDownload"

--cancelPending :: PieceIx -> Updates ()
cancelPending pix = pendingPieces %= L.filter ((pix ==) . snd)

instance Download MetadataDownload (Piece BS.ByteString) where
  scheduleBlock addr bf = do
    bkt  <- use bucket
    case spans metadataPieceSize bkt of
      []              -> return Nothing
      ((off, _ ) : _) -> do
        let pix = off `div` metadataPieceSize
        pendingPieces %= ((addr, pix) :)
        return (Just (BlockIx pix 0 metadataPieceSize))

  resetPending addr = pendingPieces %= L.filter ((addr ==) . fst)

  pushBlock addr Torrent.Piece {..} = do
    p    <- use pendingPieces
    when ((addr, pieceIndex) `L.notElem` p) $
      error "not requested"
    cancelPending pieceIndex

    bucket %= Block.insert (metadataPieceSize * pieceIndex) pieceData
    b <- use bucket
    case toPiece b of
      Nothing     -> return Nothing
      Just chunks -> do
          t <- use topic
          case parseInfoDict (BL.toStrict chunks) t of
            Right x -> do
                pendingPieces .= []
                return undefined -- (Just x)
            Left  e -> do
                pendingPieces .= []
                bucket .= Block.empty (Block.size b)
                return undefined -- Nothing
   where
      -- todo use incremental parsing to avoid BS.concat call
      parseInfoDict :: BS.ByteString -> InfoHash -> Result InfoDict
      parseInfoDict chunk topic =
        case BE.decode chunk of
          Right (infodict @ InfoDict {..})
            | topic == idInfoHash -> return infodict
            |      otherwise      -> Left "broken infodict"
          Left err -> Left $ "unable to parse infodict " ++ err

{-----------------------------------------------------------------------
--  Content download
-----------------------------------------------------------------------}
-- $content-download
--
--  A block can have one of the following status:
--
--     1) /not allowed/: Piece is not in download set.
--
--     2) /waiting/: (allowed?) Block have been allowed to download,
--     but /this/ peer did not send any 'Request' message for this
--     block. To allow some piece use
--     'Network.BitTorrent.Exchange.Selector' and then 'allowedSet'
--     and 'allowPiece'.
--
--     3) /inflight/: (pending?) Block have been requested but
--     /remote/ peer did not send any 'Piece' message for this block.
--     Related functions 'markInflight'
--
--     4) /pending/: (stalled?) Block have have been downloaded
--     Related functions 'insertBlock'.
--
--   Piece status:
--
--     1) /assembled/: (downloaded?) All blocks in piece have been
--     downloaded but the piece did not verified yet.
--
--       * Valid: go to completed;
--
--       * Invalid: go to waiting.
--
--     2) /corrupted/:
--
--     3) /downloaded/: (verified?) A piece have been successfully
--     verified via the hash. Usually the piece should be stored to
--     the 'System.Torrent.Storage' and /this/ peer should send 'Have'
--     messages to the /remote/ peers.
--

data PieceEntry = PieceEntry
  { pending :: [(PeerAddr IP, BlockIx)]
  , stalled :: Bucket
  }

pieceEntry :: PieceSize -> PieceEntry
pieceEntry s = PieceEntry [] (Block.empty s)

isEmpty :: PieceEntry -> Bool
isEmpty PieceEntry {..} = L.null pending && Block.null stalled

_holes :: PieceIx -> PieceEntry -> [BlockIx]
_holes pix PieceEntry {..} = fmap mkBlockIx (spans defaultTransferSize stalled)
  where
    mkBlockIx (off, sz) = BlockIx pix off sz

data ContentDownload = ContentDownload
  { inprogress     :: !(Map PieceIx PieceEntry)
  , bitfield       :: !Bitfield
  , pieceSize      :: !PieceSize
  , contentStorage ::  Storage
  }

contentDownload :: Bitfield -> PieceSize -> Storage -> ContentDownload
contentDownload = ContentDownload M.empty

--modifyEntry :: PieceIx -> (PieceEntry -> PieceEntry) -> DownloadUpdates ()
modifyEntry pix f = modify $ \ s @ ContentDownload {..} -> s
    { inprogress = alter (g pieceSize) pix inprogress }
  where
    g s = h . f . fromMaybe (pieceEntry s)
    h e
      | isEmpty e = Nothing
      | otherwise = Just e

instance Download ContentDownload (Block BL.ByteString) where
  scheduleBlocks n addr maskBF = do
    ContentDownload {..} <- get
    let wantPieces = maskBF `BF.difference` bitfield
    let wantBlocks = L.concat $ M.elems $ M.mapWithKey _holes $
                     M.filterWithKey (\ pix _ -> pix `BF.member` wantPieces)
                      inprogress

    bixs <- if L.null wantBlocks
      then do
        mpix <- choosePiece wantPieces
        case mpix of -- TODO return 'n' blocks
          Nothing  -> return []
          Just pix -> return [leadingBlock pix defaultTransferSize]
      else chooseBlocks wantBlocks n

    forM_ bixs $ \ bix -> do
      modifyEntry (ixPiece bix) $ \ e @ PieceEntry {..} -> e
        { pending = (addr, bix) : pending }

    return bixs
    where
      -- TODO choose block nearest to pending or stalled sets to reduce disk
      -- seeks on remote machines
      --chooseBlocks :: [BlockIx] -> Int -> DownloadUpdates [BlockIx]
      chooseBlocks xs n = return (L.take n xs)

      -- TODO use selection strategies from Exchange.Selector
      --choosePiece :: Bitfield -> DownloadUpdates (Maybe PieceIx)
      choosePiece bf
        | BF.null bf = return $ Nothing
        | otherwise  = return $ Just $ BF.findMin bf

  getRequestQueueLength addr = do
    m <- gets (M.map (L.filter ((==) addr . fst) . pending) . inprogress)
    return $ L.sum $ L.map L.length $ M.elems m

  resetPending addr = modify $ \ s -> s { inprogress = reset (inprogress s) }
    where
      reset = fmap $ \ e -> e
            { pending = L.filter (not . (==) addr . fst) (pending e) }

  pushBlock addr blk @ Block {..} = do
    mpe <- gets (M.lookup blkPiece . inprogress)
    case mpe of
      Nothing -> return Nothing
      Just (pe @ PieceEntry {..})
        | blockIx blk `L.notElem` fmap snd pending -> return Nothing
        |             otherwise                    -> do
         let bkt' = Block.insertLazy blkOffset blkData stalled
         case toPiece bkt' of
           Nothing        -> do
             modifyEntry blkPiece $ \ e @ PieceEntry {..} -> e
               { pending = L.filter ((==) (blockIx blk) . snd) pending
               , stalled = bkt'
               }
             return (Just False)

           Just pieceData -> do
             -- TODO verify
             storage <- gets contentStorage
             liftIO $ writePiece (Torrent.Piece blkPiece pieceData) storage
             modify $ \ s @ ContentDownload {..} -> s
               { inprogress =  M.delete blkPiece inprogress
               , bitfield   = BF.insert blkPiece bitfield
               }
             return (Just True)
