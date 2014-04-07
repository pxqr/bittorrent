-- | see <http://bittorrent.org/beps/bep_0020.html>
module Network.BitTorrent.Core.FingerprintSpec (spec) where
import Test.Hspec
import Network.BitTorrent.Address

spec :: Spec
spec = do
  describe "client info" $ do
    it "decode mainline encoded peer id" $ do
      fingerprint "M4-3-6--xxxxxxxxxxxx" `shouldBe` "Mainline-4.3.6"
      fingerprint "M4-20-8-xxxxxxxxxxxx" `shouldBe` "Mainline-4.20.8"

    it "decode azureus encoded peer id" $ do
      fingerprint "-AZ2060-xxxxxxxxxxxx" `shouldBe` "Azureus-2060"
      fingerprint "-BS0000-xxxxxxxxxxxx" `shouldBe` "BTSlave-0"

    it "decode Shad0w style peer id" $ do
      fingerprint "S58B-----xxxxxxxxxxx" `shouldBe` "Shadow-5.8.11"
      fingerprint "T58B-----xxxxxxxxxxx" `shouldBe` "BitTornado-5.8.11"

    it "decode bitcomet style peer id" $ do
      fingerprint "exbc01xxxxxxxxxxxxxx" `shouldBe` "BitComet-48.49"
      fingerprint "FUTB01xxxxxxxxxxxxxx" `shouldBe` "BitComet-48.49"
      fingerprint "exbc01LORDxxxxxxxxxx" `shouldBe` "BitLord-48.49"

    it "decode opera style peer id" $ do
      fingerprint "OP0123xxxxxxxxxxxxxx" `shouldBe` "Opera-123"

    it "decode ML donkey style peer id" $ do
      fingerprint "-ML2.7.2-xxxxxxxxxxx" `shouldBe` "MLdonkey-0"

-- TODO XBT, Bits on Wheels, Queen Bee, BitTyrant, TorrenTopia,
-- BitSpirit, Rufus, G3 Torrent, FlashGet