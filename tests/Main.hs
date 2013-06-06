{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Data.IntervalSet
import Data.List as L
import Data.Ord
import Data.Word

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.Bitfield as BF
import Network.BitTorrent as BT

import Encoding



instance Arbitrary IntSet where
  arbitrary = fromList <$> arbitrary

prop_completenessRange :: Bitfield -> Bool
prop_completenessRange bf = 0 <= c && c <= 1
  where
    c = completeness bf

prop_rarestInRange :: [Bitfield] -> Bool
prop_rarestInRange xs = case rarest xs of
  Just r  -> 0 <= r && r < totalCount (maximumBy (comparing totalCount) xs)
  Nothing -> True

main :: IO ()
main = defaultMain
  [ testProperty "completeness range" prop_completenessRange
  , testProperty "rarest in range"    prop_rarestInRange
  ]
