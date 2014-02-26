{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker.RPC.HTTPSpec (spec) where

import Control.Applicative
import Control.Monad
import Data.Default
import Data.List as L
import Test.Hspec

import Data.Torrent.Progress
import Network.BitTorrent.Tracker.Message as Message
import Network.BitTorrent.Tracker.RPC.HTTP

import Network.BitTorrent.Tracker.TestData


validateInfo :: AnnounceQuery -> AnnounceInfo -> Expectation
validateInfo _ Message.Failure {..} = error "validateInfo: failure"
validateInfo AnnounceQuery {..}  AnnounceInfo {..} = do
  case respComplete <|> respIncomplete of
    Nothing -> return ()
    Just n  -> n  `shouldBe` L.length (getPeerList respPeers)

spec :: Spec
spec = parallel $ do
  forM_ (L.filter isHttpTracker trackers) $ \ TrackerEntry {..} ->
    context trackerName $ do

      describe "announce" $ do
        if tryAnnounce
          then do
            it "have valid response" $ do
              withManager def $ \ mgr -> do
--                q    <- arbitrarySample
                let q = AnnounceQuery def "-HS0003-203534.37420" 6000
                        (Progress 0 0 0) Nothing Nothing (Just Started)
                info <- announce mgr trackerURI q
                validateInfo q info
          else do
            it "should fail with RequestFailed" $ do
               pending

      describe "scrape" $ do
        if tryScraping
          then do
            it "have valid response" $ do
              withManager def $ \ mgr -> do
                xs <- scrape mgr trackerURI [def]
                L.length xs `shouldSatisfy` (>= 1)
          else do
            it "should fail with ScrapelessTracker" $ do
              pending
