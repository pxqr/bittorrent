module Network.BitTorrent.Exchange.BlockSpec (spec) where
import Control.Applicative
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Exchange.Block


instance Arbitrary a => Arbitrary (Block a) where
  arbitrary = Block <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BlockIx where
  arbitrary = BlockIx <$> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = return ()