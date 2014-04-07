-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Assembler is used to build pieces from blocks. In general
--   'Assembler' should be used to handle 'Transfer' messages when
--
--   A block can have one of the following status:
--
--     1) /not allowed/: Piece is not in download set. 'null' and 'empty'.
--
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
{-# LANGUAGE TemplateHaskell #-}
module Network.BitTorrent.Exchange.Assembler
       ( -- * Assembler
         Assembler

         -- * Query
       , Network.BitTorrent.Exchange.Assembler.null
       , Network.BitTorrent.Exchange.Assembler.size

         -- *
       , Network.BitTorrent.Exchange.Assembler.empty
       , allowPiece

         -- * Debugging
       , Network.BitTorrent.Exchange.Assembler.valid
       ) where

import Control.Applicative
import Control.Lens
import Data.IntMap.Strict as IM
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.IP

import Data.Torrent
import Network.BitTorrent.Address
import Network.BitTorrent.Exchange.Block as B

{-----------------------------------------------------------------------
--  Assembler
-----------------------------------------------------------------------}

type Timestamp = ()
{-
data BlockRequest = BlockRequest
  { requestSent    :: Timestamp
  , requestedPeer  :: PeerAddr IP
  , requestedBlock :: BlockIx
  }
-}
type BlockRange = (BlockOffset, BlockSize)
type PieceMap = IntMap

data Assembler = Assembler
  { -- | A set of blocks that have been 'Request'ed but not yet acked.
    _inflight :: Map (PeerAddr IP) (PieceMap [BlockRange])

    -- | A set of blocks that but not yet assembled.
  , _pending  :: PieceMap Bucket

    -- | Used for validation of assembled pieces.
  , info      :: PieceInfo
  }

$(makeLenses ''Assembler)


valid :: Assembler -> Bool
valid = undefined

data Result a
  = Completed   (Piece a)
  | Corrupted    PieceIx
  | NotRequested PieceIx
  | Overlapped   BlockIx

null :: Assembler -> Bool
null = undefined

size :: Assembler -> Bool
size = undefined

empty :: PieceInfo -> Assembler
empty = Assembler M.empty IM.empty

allowPiece :: PieceIx -> Assembler -> Assembler
allowPiece pix a @ Assembler {..}  = over pending (IM.insert pix bkt) a
  where
    bkt = B.empty (piPieceLength info)

allowedSet :: (PeerAddr IP) -> Assembler -> [BlockIx]
allowedSet = undefined

--inflight :: PeerAddr -> BlockIx -> Assembler -> Assembler
--inflight = undefined

--  You should check if a returned by peer block is actually have
-- been requested and in-flight. This is needed to avoid "I send
-- random corrupted block" attacks.
insert :: PeerAddr IP -> Block a -> Assembler -> Assembler
insert = undefined

{-
insert :: Block a -> Assembler a -> (Assembler a, Maybe (Result a))
insert blk @ Block {..} a @ Assembler {..} = undefined
{-
  = let (pending, mpiece) = inserta blk piecePending
    in (Assembler inflightSet pending pieceInfo, f <$> mpiece)
  where
    f p = undefined
--      | checkPieceLazy pieceInfo p = Assembled p
--      |          otherwise         = Corrupted ixPiece
-}


inflightPieces :: Assembler a -> [PieceIx]
inflightPieces Assembler {..} = IM.keys piecePending

completeBlocks :: PieceIx -> Assembler a -> [Block a]
completeBlocks pix Assembler {..}  = fromMaybe [] $ IM.lookup pix piecePending

incompleteBlocks :: PieceIx -> Assembler a -> [BlockIx]
incompleteBlocks = undefined

nextBlock :: Assembler a -> Maybe (Assembler a, BlockIx)
nextBlock Assembler {..} = undefined

inserta :: Block a
        -> PieceMap [Block a]
        -> (PieceMap [Block a], Maybe (Piece a))
inserta = undefined

-}
