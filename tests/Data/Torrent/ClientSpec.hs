module Data.Torrent.ClientSpec (spec) where

import Data.Version
import Test.Hspec

import Data.Torrent.Client
import Network.BitTorrent.Core.PeerId

spec :: Spec
spec = do
  describe "client info" $ do
    it "decode mainline encoded peer id" $ do
      clientInfo "M4-3-6--xxxxxxxxxxxx" `shouldBe` "Mainline-4.3.6"
      clientInfo "M4-20-8-xxxxxxxxxxxx" `shouldBe` "Mainline-4.20.8"

    it "decode azureus encoded peer id" $ do
      clientInfo "-AZ2060-xxxxxxxxxxxx" `shouldBe` "Azureus-2060"
      clientInfo "-BS0000-xxxxxxxxxxxx" `shouldBe` "BTSlave-0"
