{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.BitTorrent.DHT.TokenSpec (spec) where
import Control.Applicative
import Data.String
import Test.Hspec
import Test.QuickCheck
import Network.BitTorrent.DHT.Token


instance Arbitrary Token where
  arbitrary = fromString <$> arbitrary

spec :: Spec
spec = return ()