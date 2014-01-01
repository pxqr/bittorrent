{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Torrent.MetainfoSpec (spec) where

import Control.Applicative
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.BEncode
import Data.Maybe
import Data.Time
import Network.URI
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Torrent.Piece
import Data.Torrent.Layout
import Data.Torrent
import Data.Torrent.LayoutSpec ()
import Network.BitTorrent.Core.NodeSpec ()

{-----------------------------------------------------------------------
--  Common
-----------------------------------------------------------------------}

data T a = T

prop_properBEncode :: Show a => BEncode a => Eq a
                   => T a -> a -> IO ()
prop_properBEncode _ expected = actual `shouldBe` Right expected
  where
    actual = decode $ BL.toStrict $ encode expected

instance Arbitrary URI where
  arbitrary = pure $ fromJust
              $ parseURI "http://exsample.com:80/123365_asd"

{-----------------------------------------------------------------------
--  Instances
-----------------------------------------------------------------------}

instance Arbitrary HashList where
  arbitrary = HashList <$> arbitrary

instance Arbitrary PieceInfo where
  arbitrary = PieceInfo <$> arbitrary <*> arbitrary

instance Arbitrary InfoDict where
  arbitrary = infoDictionary <$> arbitrary <*> arbitrary <*> arbitrary

pico :: Gen (Maybe NominalDiffTime)
pico = oneof
  [ pure Nothing
  , (Just . fromIntegral) <$> (arbitrary :: Gen Int)
  ]

instance Arbitrary Torrent where
  arbitrary = Torrent <$> arbitrary
                 <*> arbitrary <*> arbitrary    <*> arbitrary
                 <*> pico      <*> arbitrary    <*> arbitrary
                 <*> arbitrary
                 <*> arbitrary <*> pure Nothing <*> arbitrary

{-----------------------------------------------------------------------
--  Spec
-----------------------------------------------------------------------}

spec :: Spec
spec = do
  describe "FileInfo" $ do
    it "properly bencoded" $ property $
      prop_properBEncode (T :: T (FileInfo BS.ByteString))

  describe "LayoutInfo" $ do
    it "properly bencoded" $ property $
      prop_properBEncode (T :: T LayoutInfo)

  describe "Torrent" $ do
    it "property bencoded" $ property $
      prop_properBEncode (T :: T Torrent)
