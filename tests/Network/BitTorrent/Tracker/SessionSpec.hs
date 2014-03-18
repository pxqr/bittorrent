module Network.BitTorrent.Tracker.SessionSpec (spec) where
import Control.Monad
import Data.Default
import Data.List as L
import Test.Hspec

import Data.Torrent
import Network.BitTorrent.Tracker.Message
import Network.BitTorrent.Tracker.List
import Network.BitTorrent.Tracker.RPC
import Network.BitTorrent.Tracker.Session

import Config

testSession :: Bool -> (Manager -> Session -> IO ()) -> IO ()
testSession runEmpty action = do
  t <- getTestTorrent
  withManager def def $ \ m -> do
    withSession (idInfoHash (tInfoDict t)) (trackerList t) $ \ s ->
      action m s

    when runEmpty $ do
      withSession (idInfoHash (tInfoDict t)) def $ \ s ->
        action m s

spec :: Spec
spec = do
  describe "Session" $ do
    it "start new session in paused state" $ do
      testSession True $ \ _ s -> do
        status <- getStatus s
        status `shouldBe` Paused

  describe "Query" $ do
    it "change status after notify" $ do
      testSession True $ \ m s -> do
        notify m s Started
        status <- getStatus s
        status `shouldBe` Running

        notify m s Stopped
        stopped <- getStatus s
        stopped `shouldBe` Paused

    it "return non-empty list of peers" $ do
      testSession False $ \ m s -> do
        notify m s Started
        peers <- askPeers m s
        peers `shouldSatisfy` (not . L.null)
