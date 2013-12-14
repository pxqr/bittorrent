module System.Torrent.StorageSpec (spec) where
import Control.Exception
import Data.ByteString.Lazy as BL
import System.FilePath
import System.Directory
import System.IO.Unsafe
import Test.Hspec

import Data.Torrent.Layout
import Data.Torrent.Piece
import System.Torrent.Storage


layout :: FileLayout FileSize
layout =
  [ (dir </> "_a", 20)
  , (dir </> "_b", 50)
  , (dir </> "_c", 100)
  , (dir </> "_d", 5)
  ]
  where
    dir = unsafePerformIO $ getTemporaryDirectory

createLayout :: IO ()
createLayout =
  bracket (open ReadWriteEx 0 layout) close (const (return ()))

spec :: Spec
spec = before createLayout $ do
  describe "writePiece" $ do
    it "should fail gracefully on write operation in RO mode" $ do
      s <- open ReadOnly 0 layout
      writePiece (Piece 0 "") s `shouldThrow` (== StorageIsRO)
      close s

    it "should fail if piece size do not match" $ do
      withStorage ReadWrite 1 layout $ \ s ->
        writePiece (Piece 0 "") s `shouldThrow` (== InvalidSize 0)

    it "should fail on negative index" $ do
      withStorage ReadWrite 0 layout $ \ s ->
        writePiece (Piece (-1) "") s `shouldThrow` (== InvalidIndex (-1))

    it "should fail on out of upper bound index" $ do
      withStorage ReadWrite 100 layout $ \ s -> do
        let bs = BL.replicate 100 0
        writePiece (Piece 1 bs) s
        writePiece (Piece 2 bs) s `shouldThrow` (== InvalidIndex 2)

  describe "readPiece" $ do
    it "should fail on negative index" $
      withStorage ReadOnly 0 layout $ \ s ->
        readPiece (-1) s `shouldThrow` (== InvalidIndex (-1))

    it "should fail on out of upper bound index" $ do
      withStorage ReadOnly 100 layout $ \ s -> do
        _ <- readPiece 1 s
        readPiece 2 s `shouldThrow` (== InvalidIndex 2)
