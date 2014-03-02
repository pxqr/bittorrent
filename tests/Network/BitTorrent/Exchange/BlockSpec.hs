module Network.BitTorrent.Exchange.BlockSpec (spec) where
import Control.Applicative
import Control.Exception
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
  arbitrary = do
    s      <- arbitrary `suchThat` (> 0)
    chunks <- arbitrary
    return $ Block.fromList s chunks

isSomeException :: SomeException -> Bool
isSomeException = const True

spec :: Spec
spec = do
  describe "empty" $ do
    it "should fail on bad size" $ do
      evaluate (Block.empty (-1)) `shouldThrow` isSomeException

  describe "toPiece" $ do
    it "render to piece when it is full" $ property $ \ bkt ->
      full bkt == isJust (toPiece bkt)