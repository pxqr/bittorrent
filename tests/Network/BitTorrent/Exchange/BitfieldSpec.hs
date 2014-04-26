{-# OPTIONS -fno-warn-orphans #-}
module Network.BitTorrent.Exchange.BitfieldSpec (spec) where
import Control.Applicative
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Exchange.Bitfield

instance Arbitrary Bitfield where
  arbitrary = fromBitmap <$> arbitrary

spec :: Spec
spec = return ()