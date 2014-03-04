{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Exchange.SessionSpec (spec) where
import Test.Hspec

import Data.Torrent
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Session

import Config


nullLogger :: LogFun
nullLogger _ _ x _ = print x

spec :: Spec
spec = do
  describe "metadata exchange" $ do
    it "should fetch info dictionary" $ do
      withRemoteAddr $ \ addr -> do
        Torrent {..} <- getTestTorrent
        myAddr <- getMyAddr
        ses  <- newSession nullLogger myAddr "" tInfoDict
        connect addr ses
        dict <- waitMetadata ses
        closeSession ses
        dict `shouldBe` tInfoDict

    it "should serve info dictionary" $ do
      pending

  describe "content exchange" $ do
    it "should fetch torrent content" $ do
      pending

    it "should serve torrent content" $ do
      pending
