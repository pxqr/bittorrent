-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Piece selection algorithms.
--
module Network.BitTorrent.Exchange.Selection
       ( -- * Selection
         Selector
       , selector
       , strategyClass

       , strictFirst
       , strictLast
       , rarestFirst
       , randomFirst
       , endGame
       ) where

import Data.Ratio

import Network.BitTorrent.Exchange.Bitfield


type Selector =  Bitfield      -- ^ Indices of client /have/ pieces.
             ->  Bitfield      -- ^ Indices of peer /have/ pieces.
             -> [Bitfield]     -- ^ Indices of other peers /have/ pieces.
             -> Maybe PieceIx  -- ^ Zero-based index of piece to request
                               --   to, if any.

selector :: Selector       -- ^ Selector to use at the start.
         -> Ratio PieceCount
         -> Selector       -- ^ Selector to use after the client have
                           -- the C pieces.
         -> Selector       -- ^ Selector that changes behaviour based
                           -- on completeness.
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
    classify c
      |        c < threshold       = SCBeginning
      | c + 1 % numerator c < 1    = SCReady
    -- FIXME numerator have is not total count
      |          otherwise         = SCEnd


-- | Select the first available piece.
strictFirst :: Selector
strictFirst h a _ = Just $ findMin (difference a h)

-- | Select the last available piece.
strictLast :: Selector
strictLast h a _ = Just $ findMax (difference a h)

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
