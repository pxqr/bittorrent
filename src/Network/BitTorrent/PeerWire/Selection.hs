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
--     * Random first or rarest first selection - at the start.
--
--     * Rarest first selection - performed to avoid situation when
--     rarest piece is unaccessible.
--
--     * _End game_ seletion - performed after a peer has requested all
--     the subpieces of the content.
--
--   Note that BitTorrent applies the strict priority policy for
--   _subpiece_ or _blocks_ selection.
--
module Network.BitTorrent.PeerWire.Selection
       ( Selector
       , strictFirst, strictLast
       , rarestFirst, randomFirst, endGame, autoSelector
       ) where

import Network.BitTorrent.PeerWire.Block
import Network.BitTorrent.PeerWire.Message
import Network.BitTorrent.PeerWire.Bitfield


type Selector =  Bitfield      -- ^ Indices of client "have" pieces.
             ->  Bitfield      -- ^ Indices of peer "have" pieces.
             -> [Bitfield]     -- ^ Indices of other peers "have" pieces.
             -> Maybe PieceIx  -- ^ Zero-based index of piece to request
                               --   to, if any.

-- | Select the first available piece.
strictFirst :: Selector
strictFirst h a _ = findMin (difference a h)

-- | Select the last available piece.
strictLast :: Selector
strictLast h a _ = findMax (difference a h)

-- |
rarestFirst :: Selector
rarestFirst h a xs = rarest (frequencies (map (intersection want) xs))
  where
    want = difference h a
    rarest = Just . head

-- | In general random first is faster than rarest first strategy but
--    only if all pieces are available.
randomFirst :: IO Selector
randomFirst = do
--  randomIO
  error "randomFirst"

endGame :: Selector
endGame = strictLast

autoSelector :: Selector
autoSelector = undefined
