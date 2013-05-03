-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--
--   This module provides Bitfield datatype used to represent sets of
--   piece indexes any peer have. All associated operations should be
--   defined here as well.
--
module Network.BitTorrent.PeerWire.Bitfield
       ( Bitfield(..)
       , getBitfield, putBitfield, bitfieldByteCount
       ) where

import Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Serialize


newtype Bitfield = MkBitfield {
    bfBits :: ByteString
  } deriving (Show, Eq, Ord)

bitfieldByteCount :: Bitfield -> Int
bitfieldByteCount = B.length . bfBits
{-# INLINE bitfieldByteCount #-}

getBitfield :: Int -> Get Bitfield
getBitfield n = MkBitfield <$> getBytes n
{-# INLINE getBitfield #-}

putBitfield :: Bitfield -> Put
putBitfield = putByteString . bfBits
{-# INLINE putBitfield #-}