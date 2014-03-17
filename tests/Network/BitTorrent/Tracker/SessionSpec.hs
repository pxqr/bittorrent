module Network.BitTorrent.Tracker.SessionSpec (spec) where
import Data.Default
import Data.List as L
import Test.Hspec

import Data.Torrent
import Network.BitTorrent.Tracker.Message
import Network.BitTorrent.Tracker.List
import Network.BitTorrent.Tracker.RPC
import Network.BitTorrent.Tracker.Session

import Config

testSession :: (Manager -> Session -> IO ()) -> IO ()
testSession action = do
  t <- getTestTorrent
  withManager def def $ \ m ->
    withSession (idInfoHash (tInfoDict t)) (trackerList t) $ \ s ->
      action m s

spec :: Spec
spec = do
  describe "Session" $ do
    it "start new session in paused state" $ do
      testSession $ \ _ s -> do
        status <- getStatus s
        status `shouldBe` Paused

  describe "Query" $ do
    it "change status after notify" $ do
      testSession $ \ m s -> do
        notify m s Started
        status <- getStatus s
        status `shouldBe` Running

        notify m s Stopped
        stopped <- getStatus s
        stopped `shouldBe` Paused

    it "return non-empty list of peers" $ do
      testSession $ \ m s -> do
        notify m s Started
        peers <- askPeers m s
        peers `shouldSatisfy` (not . L.null)
