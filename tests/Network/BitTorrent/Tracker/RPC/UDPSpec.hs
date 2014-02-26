{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker.RPC.UDPSpec (spec, rpcOpts) where
import Control.Concurrent.Async
import Control.Monad
import Data.Default
import Data.List as L
import Data.Maybe
import Test.Hspec

import Network.BitTorrent.Core
import Network.BitTorrent.Tracker.Message as Message

import Network.BitTorrent.Tracker.TestData
import Network.BitTorrent.Tracker.MessageSpec hiding (spec)
import Network.BitTorrent.Tracker.RPC.UDP


validateInfo :: AnnounceQuery -> AnnounceInfo -> Expectation
validateInfo _ Message.Failure {..} = error "validateInfo: failure"
validateInfo AnnounceQuery {..}  AnnounceInfo {..} = do
    respComplete    `shouldSatisfy` isJust
    respIncomplete  `shouldSatisfy` isJust
    respMinInterval `shouldSatisfy` isNothing
    respWarning     `shouldSatisfy` isNothing
    peerList `shouldSatisfy` L.all (isNothing . peerId)
  where
    peerList = getPeerList respPeers

-- | Number of concurrent calls.
rpcCount :: Int
rpcCount = 100

rpcOpts :: Options
rpcOpts = def
  { optMinTimeout = 1
  , optMaxTimeout = 10
  }

spec :: Spec
spec = parallel $ do
  forM_ (L.filter isUdpTracker trackers) $ \ TrackerEntry {..} ->
    context trackerName $ do

      describe "announce" $ do
        if tryAnnounce then do
          it "have valid response" $ do
            withManager rpcOpts $ \ mgr -> do
              q <- arbitrarySample
              announce mgr trackerURI q >>= validateInfo q
        else do
          it "should throw TrackerNotResponding" $ do
            pending

      describe "scrape" $ do
        if tryScraping then do
          it "have valid response" $ do
            withManager rpcOpts $ \ mgr -> do
              xs <- scrape mgr trackerURI [def]
              L.length xs `shouldSatisfy` (>= 1)
        else do
          it "should throw TrackerNotResponding" $ do
            pending


      describe "Manager" $ do
        when tryScraping $ do
          it "should handle arbitrary intermixed concurrent queries" $ do
            withManager rpcOpts $ \ mgr -> do
              _ <- mapConcurrently (\ _ -> scrape mgr trackerURI [def]) [1..rpcCount]
              return ()