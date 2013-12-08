module Network.BitTorrent.Exchange.MessageSpec (spec) where
import Control.Applicative
import Control.Exception
import Data.ByteString as BS
import Data.Default
import Data.List as L
import Data.Set as S
import Data.Serialize as S
import Data.String
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

instance Arbitrary ProtocolString where
  arbitrary = fromString <$> (arbitrary `suchThat` ((200 <) . L.length))

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

  describe "ProtocolString" $ do
    it "fail to construct invalid string" $ do
      let str = L.replicate 500 'x'
      evaluate (fromString str :: ProtocolString)
        `shouldThrow`
        errorCall ("fromString: ProtocolString too long: " ++ str)

  describe "Handshake" $ do
    it "properly serialized" $ property $ \ hs ->
      S.decode (S.encode hs ) `shouldBe` Right (hs :: Handshake)
