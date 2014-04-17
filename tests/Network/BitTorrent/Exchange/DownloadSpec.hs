{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Exchange.DownloadSpec (spec) where
import Control.Concurrent
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Test.Hspec
import Test.QuickCheck

import Data.BEncode as BE
import Data.Torrent as Torrent
import Network.BitTorrent.Address
import Network.BitTorrent.Exchange.Download
import Network.BitTorrent.Exchange.Message

import Config
import Network.BitTorrent.CoreSpec ()


placeholderAddr :: PeerAddr IP
placeholderAddr = "0.0.0.0:0"

chunkBy :: Int -> BS.ByteString -> [BS.ByteString]
chunkBy s bs
  | BS.null bs = []
  | otherwise  = BS.take s bs : chunkBy s (BS.drop s bs)

withUpdates :: Updates s a -> IO a
withUpdates m = do
  Torrent {..} <- getTestTorrent
  let infoDictLen = fromIntegral $ BL.length $ BE.encode tInfoDict
  --mvar <- newMVar (nullStatus infoDictLen)
  --runUpdates mvar placeholderAddr m
  undefined

simulateFetch :: InfoDict -> Updates s (Maybe InfoDict)
simulateFetch dict = go
  where
    blocks = chunkBy metadataPieceSize (BL.toStrict (BE.encode dict))
    packPiece ix = Torrent.Piece ix (blocks !! ix)
    ih     = idInfoHash dict

    go = do
      mix <- scheduleBlock undefined undefined
      case mix of
        Nothing -> return Nothing
        Just ix -> do
          mdict <- pushBlock undefined (packPiece ix)
          maybe go (return . Just) mdict

spec :: Spec
spec = do
  describe "scheduleBlock" $ do
    it "never schedule the same index twice" $ do
      pending

  describe "resetPending" $ do
    it "" $ do
      pending

  describe "cancelPending" $ do
    it "must not throw an exception if cancel the same piece twice" $ do
      pending

  describe "pushBlock" $ do
    it "assemble infodict from chunks" $ do
      Torrent {..} <- getTestTorrent
      mdict <- withUpdates $ simulateFetch tInfoDict
      mdict `shouldBe` Just tInfoDict

    it "must throw an exception if block if not requested" $ do
      pending