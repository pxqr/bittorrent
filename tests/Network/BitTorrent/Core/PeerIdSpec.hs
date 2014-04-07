{-# OPTIONS -fno-warn-orphans #-}
module Network.BitTorrent.Core.PeerIdSpec (spec) where
import Control.Applicative
import Data.BEncode as BE
import Data.Text.Encoding as T
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Network.BitTorrent.Address


instance Arbitrary PeerId where
  arbitrary = oneof
    [ azureusStyle defaultClientId defaultVersionNumber
        <$> (T.encodeUtf8 <$> arbitrary)
    , shadowStyle  'X'             defaultVersionNumber
        <$> (T.encodeUtf8 <$> arbitrary)
    ]

spec :: Spec
spec = do
  describe "PeerId" $ do
    it "properly bencoded" $ do
      BE.decode "20:01234567890123456789"
        `shouldBe` Right ("01234567890123456789" :: PeerId)