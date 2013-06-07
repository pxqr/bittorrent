-- TODO tests
-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides commonly used piece seletion algorithms
--   which used to find out which one next piece to download.
--   Selectors considered to be used in the following order:
--
--     * Random first - at the start.
--
--     * Rarest first selection - performed to avoid situation when
--     rarest piece is unaccessible.
--
--     * _End game_ seletion - performed after a peer has requested all
--     the subpieces of the content.
--
--   Note that BitTorrent applies the strict priority policy for
--   /subpiece/ or /blocks/ selection.
--
module Network.BitTorrent.PeerWire.Selection
       ( Selector

       -- * Construction
       , selector, strategyClass

       -- * Strategies
       , strictFirst, strictLast
       , rarestFirst, randomFirst, endGame
       ) where

import Data.Bitfield
import Data.Ratio
import Network.BitTorrent.PeerWire.Protocol


type Selector =  Bitfield      -- ^ Indices of client /have/ pieces.
             ->  Bitfield      -- ^ Indices of peer /have/ pieces.
             -> [Bitfield]     -- ^ Indices of other peers /have/ pieces.
             -> Maybe PieceIx  -- ^ Zero-based index of piece to request
                               --   to, if any.

selector :: Selector       -- ^ Selector to use at the start.
         -> Ratio PieceCount
         -> Selector       -- ^ Selector to use after the client have the C pieces.
         -> Selector       -- ^ Selector that changes behaviour based on completeness.
selector start pt ready   h a xs =
  case strategyClass pt h of
    SCBeginning -> start h a xs
    SCReady     -> ready h a xs
    SCEnd       -> endGame h a xs

data StartegyClass
  = SCBeginning
  | SCReady
  | SCEnd
    deriving (Show, Eq, Ord, Enum, Bounded)


strategyClass :: Ratio PieceCount -> Bitfield -> StartegyClass
strategyClass threshold = classify . completeness
  where
    classify have
      |          have < threshold     = SCBeginning
      | have + 1 % numerator have < 1 = SCReady -- FIXME numerator have is not total count
      |           otherwise           = SCEnd


-- | Select the first available piece.
strictFirst :: Selector
strictFirst h a _ = findMin (difference a h)

-- | Select the last available piece.
strictLast :: Selector
strictLast h a _ = findMax (difference a h)

-- |
rarestFirst :: Selector
rarestFirst h a xs = rarest (map (intersection want) xs)
  where
    want = difference h a

-- | In average random first is faster than rarest first strategy but
--    only if all pieces are available.
randomFirst :: Selector
randomFirst = do
--  randomIO
  error "randomFirst"

endGame :: Selector
endGame = strictLast
