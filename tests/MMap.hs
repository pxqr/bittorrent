{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Word

import System.Directory
import System.IO.MMap.Fixed


boundaryTest = do
  f <- mallocTo (interval 0 1) empty
  f <- mallocTo (interval 1 2) f
  writeElem f 0 (1 :: Word8)
  writeElem f 1 (2 :: Word8)
  bs <- readBytes (interval 0 2) f
  "\x1\x2" @=? bs

mmapSingle = do
  f  <- mmapTo "single.test" (10, 5) 5 empty
  writeBytes (interval 5 5) "abcde" f
  bs <- readBytes (interval 5 5) f
  "abcde" @=? bs

coalesceTest = do
  f <- mmapTo "a.test"  (0, 1) 10 empty
  f <- mmapTo "bc.test" (0, 2) 12 f
  f <- mmapTo "c.test"  (0, 1) 13 f
  writeBytes (interval 10 4) "abcd" f
  bs <- readBytes  (interval 10 4) f
  "abcd" @=? bs

main :: IO ()
main = do
 let tmpdir =  "tmp"
 createDirectoryIfMissing True tmpdir
 setCurrentDirectory tmpdir

 defaultMain
  [ testCase "boudary"  boundaryTest
  , testCase "single"   mmapSingle
  , testCase "coalesce" coalesceTest
  ]