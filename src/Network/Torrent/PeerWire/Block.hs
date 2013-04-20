module Network.Torrent.PeerWire.Block
       ( BlockIx(..), Block(..)
       ) where

import Data.ByteString (ByteString)

data BlockIx = BlockIx {
    ixPiece  :: {-# UNPACK #-} !Int -- ^ Zero-based piece index.
  , ixOffset :: {-# UNPACK #-} !Int -- ^ Zero-based byte offset within the piece.
  , ixLength :: {-# UNPACK #-} !Int -- ^ Block size starting from offset.
  } deriving (Show, Eq)

data Block = Block {
    blkPiece  :: Int         -- ^ Zero-based piece index.
  , blkOffset :: Int         -- ^ Zero-based byte offset within the piece.
  , blkData   :: ByteString  -- ^ Payload.
  } deriving (Show, Eq)
