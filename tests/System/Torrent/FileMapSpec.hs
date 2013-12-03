-- this is test string used in the 'spec' --- don't touch me!
module System.Torrent.FileMapSpec (spec) where

import Control.Monad.Loops
import Data.List as L
import Data.ByteString.Lazy as BL
import System.Directory
import System.FilePath
import System.IO.Unsafe
import Test.Hspec

import Data.Torrent.Layout
import System.Torrent.FileMap as FM


layout :: FileLayout FileSize
layout =
  [ (dir </> "a", 2)
  , (dir </> "b", 3)
  , (dir </> "c", 2)
  ]
  where
    dir = unsafePerformIO $ getTemporaryDirectory

spec :: Spec
spec = do
  describe "FileMap" $ do
    it "creates new files" $ do
      m <- mmapFiles ReadWriteEx layout
      unmapFiles m

      (doesFileExist . fst) `allM` layout
        `shouldReturn` True

    it "have specified size" $ do
      m <- mmapFiles ReadOnly layout
      FM.size m `shouldBe` L.sum (L.map snd layout)
      unmapFiles m

    it "read from files" $ do
      let thisFile = [("tests/System/Torrent/FileMapSpec.hs", 15)]
      m <- mmapFiles ReadOnly thisFile
      readBytes 3 15 m `shouldReturn` "this is test"
      unmapFiles m

    it "writes to files" $ do
      m <- mmapFiles ReadWriteEx layout
      writeBytes 0 "a"   m
      readBytes  0 1 m `shouldReturn` "a"
      writeBytes 1 "ab"  m
      readBytes  1 2 m `shouldReturn` "ab"
      writeBytes 3 "b"   m
      readBytes  3 1 m `shouldReturn` "b"
      writeBytes 4 "bc"  m
      readBytes  4 2 m `shouldReturn` "bc"
      writeBytes 6 "c"   m
      readBytes  6 1 m `shouldReturn` "c"
      readBytes  0 7 m `shouldReturn` "aabbbcc"
      unmapFiles m

      BL.readFile (fst (layout !! 0)) `shouldReturn` "aa"
      BL.readFile (fst (layout !! 1)) `shouldReturn` "bbb"
      BL.readFile (fst (layout !! 2)) `shouldReturn` "cc"

    it "no buffer underflow errors" $ do
      m <- mmapFiles ReadOnly layout
      readBytes (-1) 1  m `shouldReturn` ""
      readBytes (-5) 12 m `shouldReturn` ""
      unmapFiles m

    it "no buffer overflow errors" $ do
      m <- mmapFiles ReadOnly layout
      writeBytes 5 "ddddddddd" m -- cause segfault
      unmapFiles m

      BL.readFile (fst (layout !! 2)) `shouldReturn` "dd"

    it "isomorphic to lazy bytestring" $ do
      m <- mmapFiles ReadOnly layout
      fromLazyByteString (toLazyByteString m) `shouldBe` m
      unmapFiles m