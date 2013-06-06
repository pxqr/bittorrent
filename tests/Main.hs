{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import qualified Data.ByteString.Lazy as Lazy
import Data.IntervalSet
import Data.List as L
import Data.Ord
import Data.Maybe
import Data.Word
import Data.Text as T
import Network.URI

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.BEncode
import Data.Bitfield as BF
import Data.Torrent
import Network.BitTorrent as BT

import Debug.Trace
import Encoding


instance Arbitrary URI where
  arbitrary = pure $ fromJust
              $ parseURI "http://exsample.com:80/123365_asd"

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

{-----------------------------------------------------------------------
    Bitfield
-----------------------------------------------------------------------}
-- other properties are tested in IntervalSet

prop_completenessRange :: Bitfield -> Bool
prop_completenessRange bf = 0 <= c && c <= 1
  where
    c = completeness bf

prop_minMax :: Bitfield -> Bool
prop_minMax bf
  | BF.null bf = True
  | otherwise  = BF.findMin bf <= BF.findMax bf

prop_rarestInRange :: [Bitfield] -> Bool
prop_rarestInRange xs = case rarest xs of
  Just r  -> 0 <= r
          && r < totalCount (maximumBy (comparing totalCount) xs)
  Nothing -> True

{- this one should give pretty good coverage -}
prop_differenceDeMorgan :: Bitfield -> Bitfield -> Bitfield -> Bool
prop_differenceDeMorgan a b c =
  (a `BF.difference` (b `BF.intersection` c))
     == ((a `BF.difference` b) `BF.union` (a `BF.difference` c))
  &&
  (a `BF.difference` (b `BF.union` c))
     == ((a `BF.difference` b) `BF.intersection` (a `BF.difference` c))


{-----------------------------------------------------------------------
    Torrent
-----------------------------------------------------------------------}

prop_properBEncode :: Show a => BEncodable a => Eq a => T a -> a -> Bool
prop_properBEncode _ expected = actual == Right expected
  where
    actual = decoded $ Lazy.toStrict $ encoded expected


-- TODO tests for torrent: encoding <-> decoding
instance Arbitrary FileInfo where
  arbitrary = FileInfo <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ContentInfo where
  arbitrary = oneof
    [ SingleFile <$> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> arbitrary <*> arbitrary
    , MultiFile  <$> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Torrent where
  arbitrary = torrent <$> arbitrary
                 <*> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> pure Nothing <*> arbitrary

main :: IO ()
main = defaultMain
  [ testProperty "completeness range"      prop_completenessRange
  , testProperty "rarest in range"         prop_rarestInRange
  , testProperty "min less that max"       prop_minMax
  , testProperty "difference de morgan"    prop_differenceDeMorgan

  , testProperty "file info encoding"      $
      prop_properBEncode (T :: T FileInfo)
  , testProperty "content info encoding"   $
      prop_properBEncode (T :: T ContentInfo)
  , testProperty "torrent encoding" $
      prop_properBEncode (T :: T Torrent)
  ]
