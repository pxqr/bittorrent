module Network.BitTorrent.Exchange.Session.Status
       ( -- * Environment
         StatusUpdates
       , runStatusUpdates

         -- * Status
       , SessionStatus
       , sessionStatus

         -- * Query
       , getBitfield
       , getRequestQueueLength

         -- * Control
       , scheduleBlocks
       , resetPending
       , pushBlock
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.State
import Data.ByteString.Lazy as BL
import Data.Default
import Data.List as L
import Data.Maybe
import Data.Map as M
import Data.Set as S
import Data.Tuple

import Data.Torrent
import Network.BitTorrent.Exchange.Bitfield as BF
import Network.BitTorrent.Address
import Network.BitTorrent.Exchange.Block as Block
import System.Torrent.Storage (Storage, writePiece)


{-----------------------------------------------------------------------
--  Piece entry
-----------------------------------------------------------------------}

data PieceEntry = PieceEntry
  { pending :: [(PeerAddr IP, BlockIx)]
  , stalled :: Bucket
  }

pieceEntry :: PieceSize -> PieceEntry
pieceEntry s = PieceEntry [] (Block.empty s)

isEmpty :: PieceEntry -> Bool
isEmpty PieceEntry {..} = L.null pending && Block.null stalled

holes :: PieceIx -> PieceEntry -> [BlockIx]
holes pix PieceEntry {..} = fmap mkBlockIx (spans defaultTransferSize stalled)
  where
    mkBlockIx (off, sz) = BlockIx pix off sz

{-----------------------------------------------------------------------
--  Session status
-----------------------------------------------------------------------}

data SessionStatus = SessionStatus
  { inprogress :: !(Map PieceIx PieceEntry)
  , bitfield   :: !Bitfield
  , pieceSize  :: !PieceSize
  }

sessionStatus :: Bitfield -> PieceSize -> SessionStatus
sessionStatus bf ps = SessionStatus
  { inprogress = M.empty
  , bitfield   = bf
  , pieceSize  = ps
  }

type StatusUpdates a = StateT SessionStatus IO a

-- |
runStatusUpdates :: MVar SessionStatus -> StatusUpdates a -> IO a
runStatusUpdates var m = modifyMVar var (fmap swap . runStateT m)

getBitfield :: MVar SessionStatus -> IO Bitfield
getBitfield var = bitfield <$> readMVar var

getRequestQueueLength :: PeerAddr IP -> StatusUpdates Int
getRequestQueueLength addr = do
  m <- gets (M.elems . M.map (L.filter ((==) addr . fst) . pending) . inprogress)
  return $ L.sum $ L.map L.length m

modifyEntry :: PieceIx -> (PieceEntry -> PieceEntry) -> StatusUpdates ()
modifyEntry pix f = modify $ \ s @ SessionStatus {..} -> s
    { inprogress = alter (g pieceSize) pix inprogress }
  where
    g s = h . f . fromMaybe (pieceEntry s)
    h e
      | isEmpty e = Nothing
      | otherwise = Just e

{-----------------------------------------------------------------------
--  Piece download
-----------------------------------------------------------------------}

-- TODO choose block nearest to pending or stalled sets to reduce disk
-- seeks on remote machines
chooseBlocks :: [BlockIx] -> Int -> StatusUpdates [BlockIx]
chooseBlocks xs n = return (L.take n xs)

-- TODO use selection strategies from Exchange.Selector
choosePiece :: Bitfield -> StatusUpdates (Maybe PieceIx)
choosePiece bf
  | BF.null bf = return $ Nothing
  | otherwise  = return $ Just $ BF.findMin bf

scheduleBlocks :: PeerAddr IP -> Bitfield -> Int -> StatusUpdates [BlockIx]
scheduleBlocks addr maskBF n = do
  SessionStatus {..} <- get
  let wantPieces = maskBF `BF.difference` bitfield
  let wantBlocks = L.concat $ M.elems $ M.mapWithKey holes $
                   M.filterWithKey (\ pix _ -> pix `BF.member` wantPieces) inprogress

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


-- | Remove all pending block requests to the remote peer. May be used
-- when:
--
--     * a peer closes connection;
--
--     * remote peer choked this peer;
--
--     * timeout expired.
--
resetPending :: PeerAddr IP -> StatusUpdates ()
resetPending addr = modify $ \ s -> s { inprogress = reset (inprogress s) }
  where
    reset = fmap $ \ e -> e
            { pending = L.filter (not . (==) addr . fst) (pending e) }

-- | MAY write to storage, if a new piece have been completed.
pushBlock :: Block BL.ByteString -> Storage -> StatusUpdates (Maybe Bool)
pushBlock blk @ Block {..} storage = do
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
           liftIO $ writePiece (Piece blkPiece pieceData) storage
           modify $ \ s @ SessionStatus {..} -> s
             { inprogress =  M.delete blkPiece inprogress
             , bitfield   = BF.insert blkPiece bitfield
             }
           return (Just True)
