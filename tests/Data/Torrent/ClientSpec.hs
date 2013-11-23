-- | see <http://bittorrent.org/beps/bep_0020.html>
module Data.Torrent.ClientSpec (spec) where
import Test.Hspec
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

    it "decode Shad0w style peer id" $ do
      clientInfo "S58B-----xxxxxxxxxxx" `shouldBe` "Shadow-5.8.11"
      clientInfo "T58B-----xxxxxxxxxxx" `shouldBe` "BitTornado-5.8.11"

    it "decode bitcomet style peer id" $ do
      clientInfo "exbc01xxxxxxxxxxxxxx" `shouldBe` "BitComet-48.49"
      clientInfo "FUTB01xxxxxxxxxxxxxx" `shouldBe` "BitComet-48.49"
      clientInfo "exbc01LORDxxxxxxxxxx" `shouldBe` "BitLord-48.49"