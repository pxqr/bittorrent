{-# OPTIONS -fno-warn-orphans #-}
module Data.Torrent.MagnetSpec (spec) where

import Control.Applicative
import Data.Maybe
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Network.URI

import Data.Torrent.InfoHash
import Data.Torrent.Magnet
import Data.Torrent.InfoHashSpec ()


instance Arbitrary URIAuth where
  arbitrary = URIAuth <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary URI where
  arbitrary
    = pure $ fromJust $ parseURI "http://ietf.org/1737.txt?a=1&b=h#123"

instance Arbitrary Magnet where
  arbitrary = Magnet <$> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> pure (error "arbitrary magnet")

magnetEncoding :: Magnet -> Bool
magnetEncoding m = parseMagnet (renderMagnet m) == Just m

spec :: Spec
spec = do
  describe "Magnet" $ do
    it "properly encoded" $ property $ magnetEncoding

    it "parse base32" $ do
      let magnet = "magnet:?xt=urn:btih:CT76LXJDDCH5LS2TUHKH6EUJ3NYKX4Y6"
      let ih = InfoHash "\DC4\255\229\221#\CAN\143\213\203S\161\212\DEL\DC2\137\219p\171\243\RS"
      parseMagnet magnet `shouldBe` Just (nullMagnet ih)

    it "parse base16" $ do
      let magnet = "magnet:?xt=urn:btih:0123456789abcdef0123456789abcdef01234567"
      let ih     = InfoHash "\SOH#Eg\137\171\205\239\SOH#Eg\137\171\205\239\SOH#Eg"
      parseMagnet magnet `shouldBe` Just (nullMagnet ih)
