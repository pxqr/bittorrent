module Data.Torrent.ClientSpec (spec) where

import Data.Version
import Test.Hspec

import Data.Torrent.Client
import Network.BitTorrent.Core.PeerId

spec :: Spec
spec = do
  describe "client info" $ do
    it "decode azureus encoded peer id" $ do
      clientInfo "-AZ2060-xxxxxxxxxxxx" `shouldBe`
        ClientInfo IAzureus (ClientVersion (Version [2060] []))
      clientInfo "-BS0000-xxxxxxxxxxxx" `shouldBe`
        ClientInfo IBTSlave (ClientVersion (Version [0] []))
