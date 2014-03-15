{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Exchange.SessionSpec (spec) where
import Test.Hspec

import Data.Torrent
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Session

import Config


nullLogger :: LogFun
nullLogger _ _ x _ = print x

simpleSession :: InfoDict -> (Session -> IO ()) -> IO ()
simpleSession dict action = do
  withRemoteAddr $ \ addr -> do
    myAddr <- getMyAddr
    ses  <- newSession nullLogger myAddr "" dict
    connect addr ses
    action ses
    closeSession ses

spec :: Spec
spec = do
  describe "construction" $ do
    describe "newSession" $ do
      it "" $ do
        pending

    describe "closeSession" $ do
      it "" $ do
        pending

  describe "connection set" $ do
    describe "connect" $ do
      it "" $ do
        pending

    describe "establish" $ do
      it "" $ do
        pending

  describe "exchange" $ do
    describe "metadata" $ do
      it "should fetch info dictionary" $ do
        Torrent {..} <- getTestTorrent
        simpleSession tInfoDict $ \ ses -> do
          dict <- waitMetadata ses
          dict `shouldBe` tInfoDict

      it "should serve info dictionary" $ do
        pending

    describe "content" $ do
      it "should fetch torrent content" $ do
        Torrent {..} <- getTestTorrent
        simpleSession tInfoDict $ \ ses -> do
          pending
--        st <- waitData ses
--        verifyStorage st (idPieceInfo tInfoDict)

      it "should serve torrent content" $ do
        pending
