module Network.BitTorrent.DHT.RoutingSpec (spec) where
import Data.Default
import Test.Hspec

import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Routing as T

spec :: Spec
spec = do
  describe "size" $ do
    it "null table is empty" $ do
      T.size (nullTable def :: Table IPv4) `shouldBe` 0