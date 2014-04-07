{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.BitTorrent.Exchange.MessageSpec (spec) where
import Control.Applicative
import Control.Exception
import Data.ByteString as BS
import Data.List as L
import Data.Set as S
import Data.Serialize as S
import Data.String
import Test.Hspec
import Test.QuickCheck

import Data.TorrentSpec ()
import Data.Torrent.BitfieldSpec ()
import Network.BitTorrent.CoreSpec ()
import Network.BitTorrent.Address ()
import Network.BitTorrent.Exchange.BlockSpec ()
import Network.BitTorrent.Exchange.Message

instance Arbitrary Extension where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Caps where
  arbitrary = toCaps <$> arbitrary

instance Arbitrary ExtendedExtension where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ExtendedCaps where
  arbitrary = toCaps <$> arbitrary

instance Arbitrary ProtocolName where
  arbitrary = fromString <$> (arbitrary `suchThat` ((200 <) . L.length))

instance Arbitrary Handshake where
  arbitrary = Handshake <$> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary

instance Arbitrary StatusUpdate where
  arbitrary = frequency
    [ (1, Choking    <$> arbitrary)
    , (1, Interested <$> arbitrary)
    ]

instance Arbitrary Available where
  arbitrary = frequency
    [ (1, Have     <$> arbitrary)
    , (1, Bitfield <$> arbitrary)
    ]

instance Arbitrary Transfer where
  arbitrary = frequency
    [ (1, Request <$> arbitrary)
    , (1, Piece   <$> arbitrary)
    , (1, Cancel  <$> arbitrary)
    ]

instance Arbitrary FastMessage where
  arbitrary = frequency
    [ (1, pure HaveAll)
    , (1, pure HaveNone)
    , (1, SuggestPiece  <$> arbitrary)
    , (1, RejectRequest <$> arbitrary)
    , (1, AllowedFast   <$> arbitrary)
    ]

instance Arbitrary Message where
  arbitrary = frequency
    [ (1, pure KeepAlive)
    , (1, Status    <$> arbitrary)
    , (1, Available <$> arbitrary)
    , (1, Transfer  <$> arbitrary)
    , (1, Fast      <$> arbitrary)
    ]

-- TODO test extension protocol

spec :: Spec
spec = do
  describe "Caps" $ do
    it "set-like container" $ property $ \ exts ->
      L.all (`allowed` (toCaps exts :: Caps)) exts

    it "preserve items" $ property $ \ extSet ->
      S.fromList (fromCaps (toCaps (S.toList extSet) :: Caps))
        `shouldBe` extSet

  describe "ByteStats" $ do
    it "preserve size" $ property $ \ msg ->
      byteLength (stats msg) `shouldBe`
        fromIntegral (BS.length (S.encode (msg :: Message)))

  describe "ProtocolName" $ do
    it "fail to construct invalid string" $ do
      let str = L.replicate 500 'x'
      evaluate (fromString str :: ProtocolName)
        `shouldThrow`
        errorCall ("fromString: ProtocolName too long: " ++ str)

  describe "Handshake" $ do
    it "properly serialized" $ property $ \ hs ->
      S.decode (S.encode hs ) `shouldBe` Right (hs :: Handshake)
