{-# OPTIONS -fno-warn-orphans #-}
module Network.BitTorrent.Internal.ProgressSpec (spec) where
import Control.Applicative
import Test.Hspec
import Test.QuickCheck
import Network.BitTorrent.Internal.Progress


instance Arbitrary Progress where
  arbitrary = Progress <$> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = return ()
