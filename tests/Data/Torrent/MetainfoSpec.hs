{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Torrent.MetainfoSpec (spec) where

import Control.Applicative
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.BEncode
import Data.Maybe
import Network.URI
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Torrent.Layout
import Data.Torrent


{-----------------------------------------------------------------------
--  Common
-----------------------------------------------------------------------}

data T a = T

prop_properBEncode :: Show a => BEncode a => Eq a
                   => T a -> a -> Bool
prop_properBEncode _ expected = actual == Right expected
  where
    actual = decode $ BL.toStrict $ encode expected

instance Arbitrary URI where
  arbitrary = pure $ fromJust
              $ parseURI "http://exsample.com:80/123365_asd"

{-----------------------------------------------------------------------
--  Instances
-----------------------------------------------------------------------}

instance Arbitrary FileSize where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Int)

instance Arbitrary a => Arbitrary (FileInfo a) where
  arbitrary = FileInfo <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LayoutInfo where
  arbitrary = oneof
    [ SingleFile <$> arbitrary
    , MultiFile  <$> arbitrary <*> arbitrary
    ]

instance Arbitrary InfoDict where
  arbitrary = undefined

instance Arbitrary Torrent where
  arbitrary = Torrent <$> arbitrary
                 <*> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> arbitrary <*> arbitrary
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
