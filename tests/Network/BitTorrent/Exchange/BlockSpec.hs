module Network.BitTorrent.Exchange.BlockSpec (spec) where
import Control.Applicative
import Data.Maybe
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Exchange.Block


instance Arbitrary a => Arbitrary (Block a) where
  arbitrary = Block <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BlockIx where
  arbitrary = BlockIx <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Bucket where
  arbitrary = error "arbitrary: block bucket"

instance Show Bucket where
  show = error "show: bucket"

spec :: Spec
spec = do
  describe "bucket" $ do
    it "render to piece when it is full" $ property $ \ bkt ->
      full bkt == isJust (toPiece bkt)