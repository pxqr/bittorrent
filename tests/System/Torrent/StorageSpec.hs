module System.Torrent.StorageSpec (spec) where
import Control.Exception
import Data.ByteString.Lazy as BL
import Data.Conduit as C
import Data.Conduit.List as C
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

psize :: PieceSize
psize = 16

pcount :: PieceCount
pcount = 11

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
        writePiece (Piece 0 bs) s

        let bs' = BL.replicate 75 0
        writePiece (Piece 1 bs') s

        writePiece (Piece 2 bs') s `shouldThrow` (== InvalidIndex 2)

  describe "readPiece" $ do
    it "should fail on negative index" $
      withStorage ReadOnly 0 layout $ \ s ->
        readPiece (-1) s `shouldThrow` (== InvalidIndex (-1))

    it "should fail on out of upper bound index" $ do
      withStorage ReadOnly 100 layout $ \ s -> do
        _ <- readPiece 1 s
        readPiece 2 s `shouldThrow` (== InvalidIndex 2)

  describe "sourceStorage" $ do
    it "should source all chunks" $ do
      withStorage ReadOnly psize layout $ \ s -> do
        n <- sourceStorage s $$ C.fold (\ n _ -> succ n) 0
        n `shouldBe` pcount

  -- this test should fail if 'sourceStorage' test fail
  describe "sinkStorage" $ do
    it "should write all chunks" $ do
      let byteVal       = 0
      let bzeroPiece  p = p { pieceData = BL.replicate (BL.length (pieceData p)) byteVal }
      let isZeroPiece p = (== byteVal) `BL.all` pieceData p

      withStorage ReadWrite psize layout $ \ s -> do
        sourceStorage s $= C.map bzeroPiece $$ sinkStorage s
        b <- sourceStorage s $$ C.fold (\ b p -> b && isZeroPiece p) True
        b `shouldBe` True
