module Network.BitTorrent.Exchange.Assembler
       ( Assembler
       ) where

import Control.Applicative
import Data.IntMap.Strict as IM
import Data.List as L
import Data.Maybe

import Data.Torrent.Piece
import Network.BitTorrent.Exchange.Block


type PieceMap = IntMap

data Assembler a = Assembler
  { inflightSet  :: PieceMap [BlockIx]
  , piecePending :: PieceMap [Block a]
  , pieceInfo    :: PieceInfo
  }

data Result a
  = Assembled   (Piece a)
  | Corrupted    PieceIx
  | NotRequested PieceIx
  | Overlapped   BlockIx

empty :: PieceInfo -> Assembler a
empty = Assembler IM.empty IM.empty

inflightPieces :: Assembler a -> [PieceIx]
inflightPieces Assembler {..} = keys piecePending

completeBlocks :: PieceIx -> Assembler a -> [Block a]
completeBlocks pix Assembler {..}  = fromMaybe [] $ IM.lookup pix piecePending

incompleteBlocks :: PieceIx -> Assembler a -> [BlockIx]
incompleteBlocks = undefined

nextBlock :: Assembler a -> Maybe (Assembler a, BlockIx)
nextBlock Assembler {..} = undefined

allowPiece :: PieceIx -> Assembler a -> Assembler a
allowPiece = undefined

insert' :: Block a -> [Block a] -> [Block a]
insert' a (x : xs) = undefined

insertBlock :: Block a -> [Block a] -> Either [Block a] (Piece a)
insertBlock  = undefined

inserta :: Block a -> PieceMap [Block a] -> (PieceMap [Block a], Maybe (Piece a))
inserta = undefined

-- | You should check if a returned by peer block is actually have
-- been requested and in-flight. This is needed to avoid "I send
-- random corrupted block" attacks.
insert :: Block a -> Assembler a -> (Assembler a, Maybe (Result a))
insert blk @ Block {..} a @ Assembler {..}
  = let (pending, mpiece) = inserta blk piecePending
    in (Assembler inflightSet pending pieceInfo, f <$> mpiece)
  where
    f p = undefined
--      | checkPieceLazy pieceInfo p = Assembled p
--      |          otherwise         = Corrupted ixPiece
