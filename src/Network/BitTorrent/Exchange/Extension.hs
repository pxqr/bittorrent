-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides peer capabilities detection.
--
--   See <http://www.bittorrent.org/beps/bep_0004.html> for more
--   information.
--
module Network.BitTorrent.Exchange.Extension
       ( -- * Capabilities
         Caps

         -- * Extensions
       , Extension(..)
       ) where

import Data.Bits
import Data.Default
import Data.Monoid
import Data.Word
import Text.PrettyPrint
import Text.PrettyPrint.Class

class (Enum a, Bounded a) => Capability a where
  capMask :: a -> Word64
  capRequires :: a -> Word64

newtype Caps a = Caps Word64

instance (Pretty a, Capability a) => Pretty (Caps a) where
  pretty = hcat . punctuate ", " . map pretty . toList

instance Default (Caps a) where
  def = Caps 0
  {-# INLINE def #-}

instance Monoid (Caps a) where
  mempty  = Caps (-1)
  {-# INLINE mempty #-}

  mappend (Caps a) (Caps b) = Caps (a .&. b)
  {-# INLINE mappend #-}

allowed :: Capability a => a -> Caps a -> Bool
allowed = member
fromList :: Capability a => [a] -> Caps a
fromList = Caps . foldr (.&.) 0 . map capMask

toList :: Capability a => Caps a -> [a]
toList (Caps rb) = filter (testMask rb . capMask) [minBound..maxBound]
  where
    testMask bits x = bits .&. x > 0


data Extension
  = ExtDHT  -- ^ BEP 5
  | ExtFast -- ^ BEP 6
    deriving (Show, Eq, Ord, Enum, Bounded)

instance Pretty Extension where
  pretty ExtDHT  = "DHT"
  pretty ExtFast = "Fast Extension"

instance Capability Extension where
  capMask ExtDHT  = 0x01
  capMask ExtFast = 0x04
