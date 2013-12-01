module Network.BitTorrent.Exchange.Assembler
       ( Assembler
       , insert

         -- * Query
       , pendingPieces
       , completeBlocks
       , incompleteBlocks
       ) where

import Data.IntMap.Strict as IM
import Data.Maybe
import Data.Torrent.Piece
import Data.Torrent.Block

type PieceMap = IntMap

-- TODO move to Data.Torrent.Piece ?
-- assembler is also a block selector?
data Assembler a = Assembler
  { piecePending :: PieceMap [Block a]
  , pieceInfo    :: PieceInfo
  }


data Result a
  = Assembled (Piece a)
  | Failed     PieceIx

-- | You should check if a returned by peer block is actually have
-- been requested and in-flight. This is needed to avoid "I send
-- random corrupted block" attacks.
insert :: Block a -> Assembler a -> (Assembler a, Maybe (Result a))
insert Block {..} Assembler {..} = undefined
--  updateWithKey bixPiece

pendingPieces :: Assembler a -> [PieceIx]
pendingPieces Assembler {..} = keys piecePending

completeBlocks :: Assembler a -> PieceIx -> [Block a]
completeBlocks Assembler {..} pix = fromMaybe [] $ IM.lookup pix piecePending

incompleteBlocks :: Assembler a -> PieceIx -> [BlockIx]
incompleteBlocks = undefined

-- TODO merge BlockSelector with Assembler?
data BlockSelector a = BlockSelector
  { assembler   :: Assembler a -- we do not select already transfered blocks
  , inflightSet :: Set BlockIx -- we do not select blocks in flight
  }

insert :: BlockSelector -> (BlockSelector a, Maybe (Result a))
insert = undefined


data StorageAdapter = StorageAdapter
  { bitfield :: Bitfield
  , requestQ :: Queue PieceIx
  }
-- we do select 'incompleteBlocks' that is not in flight

--assembler :: Assembler -> Conduit (Block a) (Result a)
--assembler = undefined

-- by priority
--   foreign request queue (max queue size)
--   assembler block information (max queue size)
--   selection strategies (bitfields)

-- when in flight queue is full we do not use selector
--   in flight queue information (max queue size)

-- piece select is used when
data PieceSelector = Selector
  { forceQueue        :: TVar (Queue PieceIx)
  , forcePendingQueue :: TVar (Queue PieceIx)
  , assembler :: TVar Assembler
  , strategy  :: Bool
  }

select :: Selector -> (Selector, PieceIx)
select = undefined