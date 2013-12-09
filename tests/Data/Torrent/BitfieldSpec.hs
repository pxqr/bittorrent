{-# OPTIONS -fno-warn-orphans #-}
module Data.Torrent.BitfieldSpec (spec) where
import Control.Applicative
import Test.Hspec
import Test.QuickCheck

import Data.Torrent.Bitfield

instance Arbitrary Bitfield where
  arbitrary = fromBitmap <$> arbitrary

spec :: Spec
spec = return ()