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

-- TODO tests for torrent: encoding <-> decoding


main :: IO ()
main = defaultMain
  [ testProperty "completeness range"      prop_completenessRange
  , testProperty "rarest in range"         prop_rarestInRange
  , testProperty "min less that max"       prop_minMax
  , testProperty "difference de morgan"    prop_differenceDeMorgan
  ]
