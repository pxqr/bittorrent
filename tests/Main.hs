{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Word
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Encoding

import Data.Bitfield as BT
import Network.BitTorrent as BT

main :: IO ()
main = defaultMain []
