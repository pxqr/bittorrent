{-# OPTIONS -fno-warn-orphans #-}
module Data.Torrent.MagnetSpec (spec) where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Network.URI

import Data.Torrent
import Data.Torrent.InfoHashSpec ()


instance Arbitrary URIAuth where
  arbitrary = URIAuth <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary URI where
  arbitrary
    = pure $ fromJust $ parseURI "http://ietf.org/1737.txt?a=1&b=h#123"

instance Arbitrary Magnet where
  arbitrary = Magnet <$> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> pure mempty

magnetEncoding :: Magnet -> IO ()
magnetEncoding m = parseMagnet (renderMagnet m) `shouldBe` Just m

spec :: Spec
spec = do
  describe "Magnet" $ do
    it "properly encoded" $ property $ magnetEncoding

    it "parse base32" $ do
      let magnet = "magnet:?xt=urn:btih:CT76LXJDDCH5LS2TUHKH6EUJ3NYKX4Y6"
      let ih = "CT76LXJDDCH5LS2TUHKH6EUJ3NYKX4Y6"
      parseMagnet magnet `shouldBe` Just (nullMagnet ih)

    it "parse base16" $ do
      let magnet = "magnet:?xt=urn:btih:0123456789abcdef0123456789abcdef01234567"
      let ih = "0123456789abcdef0123456789abcdef01234567"
      parseMagnet magnet `shouldBe` Just (nullMagnet ih)
