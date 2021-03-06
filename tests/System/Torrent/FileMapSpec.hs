-- this is test string used in the 'spec' --- don't touch me!
module System.Torrent.FileMapSpec (spec) where

import Control.Monad.Loops
import Data.List as L
import Data.ByteString.Lazy as BL
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Hspec

import Data.Torrent
import System.Torrent.FileMap as FM


withLayout :: (FileLayout FileSize -> IO ()) -> IO ()
withLayout f = do
  tmp <- getTemporaryDirectory
  withTempDirectory tmp "bittorrentTestDir" $ \dir ->
    f [ (dir </> "a", 2)
      , (dir </> "b", 3)
      , (dir </> "c", 2)
      ] `seq` return ()

spec :: Spec
spec = do
  describe "mmapFiles" $ do
    it "creates new files" $ withLayout $ \layout -> do
      m <- mmapFiles ReadWriteEx layout
      unmapFiles m

      (doesFileExist . fst) `allM` layout
        `shouldReturn` True

  describe "size" $ do
    it "is equal to the layout size" $ withLayout $ \layout -> do
      m <- mmapFiles ReadOnly layout
      FM.size m `shouldBe` L.sum (L.map snd layout)
      unmapFiles m

  describe "readBytes" $ do
    it "read from files" $ do
      let thisFile = [("tests/System/Torrent/FileMapSpec.hs", 15)]
      m <- mmapFiles ReadOnly thisFile
      readBytes 3 15 m `shouldReturn` "this is test"
      unmapFiles m

    it "ignore underflow reads" $ withLayout $ \layout -> do
      m <- mmapFiles ReadOnly layout
      readBytes (-1) 1  m `shouldReturn` ""
      readBytes (-5) 12 m `shouldReturn` ""
      unmapFiles m

    it "crop overflow reads" $ withLayout $ \layout -> do
      _m <- mmapFiles ReadWrite layout
      writeBytes 5 "cc" _m
      unmapFiles _m

      m <- mmapFiles ReadOnly layout
      readBytes 5 10 m `shouldReturn` "cc"
      unmapFiles m

  describe "writeBytes" $ do
    it "writes to files" $ withLayout $ \layout -> do
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

    let max_page_size = 4 * 1024 * 1024
    let long_bs = BL.replicate (fromIntegral max_page_size) 0

    it "no buffer underflow errors" $ withLayout $ \layout -> do
      m <- mmapFiles ReadWrite layout
      writeBytes (1 - max_page_size) long_bs m
      unmapFiles m

    it "no buffer overflow errors" $ withLayout $ \layout -> do
      m <- mmapFiles ReadWrite layout
      writeBytes 5 long_bs m
      unmapFiles m

    it "ignore underflow writes" $ withLayout $ \layout -> do
      _m <- mmapFiles ReadWrite layout
      writeBytes 0 "aa" _m
      unmapFiles _m

      m <- mmapFiles ReadWrite layout
      writeBytes (-1) "hhh" m
      unmapFiles m
      BL.readFile (fst (layout !! 0)) `shouldReturn` "aa"

    it "crop overflow writes" $ withLayout $ \layout -> do
      m <- mmapFiles ReadWrite layout
      writeBytes 5 "ddddddddd" m
      unmapFiles m
      BL.readFile (fst (layout !! 2)) `shouldReturn` "dd"

  describe "from/to lazy bytestring" $ do
    it "isomorphic to lazy bytestring" $ withLayout $ \layout -> do
      m <- mmapFiles ReadOnly layout
      fromLazyByteString (toLazyByteString m) `shouldBe` m
      unmapFiles m
