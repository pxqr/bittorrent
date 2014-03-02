module Network.BitTorrent.Exchange.BlockSpec (spec) where
import Control.Applicative
import Data.Maybe
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Network.BitTorrent.Exchange.Block as Block


instance Arbitrary a => Arbitrary (Block a) where
  arbitrary = Block <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BlockIx where
  arbitrary = BlockIx <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Bucket where
  arbitrary = Block.fromList <$> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "bucket" $ do
    it "render to piece when it is full" $ property $ \ bkt ->
      full bkt == isJust (toPiece bkt)