module Network.BitTorrent.Exchange.MessageSpec (spec) where
import Control.Applicative
import Data.ByteString as BS
import Data.Default
import Data.List as L
import Data.Set as S
import Data.Serialize as S
import Test.Hspec
import Test.QuickCheck

import Data.Torrent.InfoHashSpec ()
import Network.BitTorrent.CoreSpec ()
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Message

instance Arbitrary Extension where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Caps where
  arbitrary = toCaps <$> arbitrary

instance Arbitrary Handshake where
  arbitrary = Handshake <$> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "Caps" $ do
    it "set-like container" $ property $ \ exts ->
      L.all (`allowed` (toCaps exts :: Caps)) exts

    it "preserve items" $ property $ \ extSet ->
      S.fromList (fromCaps (toCaps (S.toList extSet) :: Caps))
        `shouldBe` extSet

  describe "Handshake" $ do
    it "properly serialized" $ property $ \ hs ->
      S.decode (S.encode hs ) `shouldBe` Right (hs :: Handshake)

    it "fail if protocol string is too long" $ do
      pid <- genPeerId
      let hs = (defaultHandshake def pid) {hsProtocol = BS.replicate 256 0}
      S.decode (S.encode hs) `shouldBe` Right hs