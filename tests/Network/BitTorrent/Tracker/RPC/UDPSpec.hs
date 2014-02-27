{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker.RPC.UDPSpec (spec, rpcOpts) where
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
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

isTimeoutExpired :: RpcException -> Bool
isTimeoutExpired (TimeoutExpired _) = True
isTimeoutExpired  _                 = False

isSomeException :: SomeException -> Bool
isSomeException _ = True

isIOException :: IOException -> Bool
isIOException _ = True

spec :: Spec
spec = parallel $ do
  describe "newManager" $ do
    it "should throw exception on zero optMaxPacketSize" $ do
      let opts = def { optMaxPacketSize = 0 }
      newManager opts `shouldThrow` isSomeException

    it "should throw exception on zero optMinTimout" $ do
      let opts = def { optMinTimeout = 0 }
      newManager opts `shouldThrow` isSomeException

    it "should throw exception on zero optMaxTimeout" $ do
      let opts = def { optMaxTimeout = 0 }
      newManager opts `shouldThrow` isSomeException

    it "should throw exception on maxTimeout < minTimeout" $ do
      let opts = def { optMinTimeout = 2, optMaxTimeout = 1 }
      newManager opts `shouldThrow` isSomeException

    it "should throw exception on  optMultiplier" $ do
      let opts = def { optMultiplier = 0 }
      newManager opts `shouldThrow` isSomeException

  describe "closeManager" $ do
    it "unblock rpc calls" $ do
      mgr <- newManager rpcOpts
      _   <- forkIO $ do
        threadDelay 10000000
        closeManager mgr
      q <- arbitrarySample
      announce mgr (trackerURI badTracker) q `shouldThrow` (== ManagerClosed)

    it "announce throw exception after manager closed" $ do
      mgr <- newManager rpcOpts
      closeManager mgr
      q <- arbitrarySample
      announce mgr (trackerURI badTracker) q `shouldThrow` isIOException

    it "scrape throw exception after manager closed" $ do
      mgr <- newManager rpcOpts
      closeManager mgr
      scrape mgr (trackerURI badTracker) [def] `shouldThrow` isIOException

  describe "withManager" $ do
    it "closesManager at exit" $ do
      mgr <- withManager rpcOpts return
      scrape mgr (trackerURI badTracker) [def] `shouldThrow` isSomeException

  describe "RPC" $ do
    forM_ (L.filter isUdpTracker trackers) $ \ TrackerEntry {..} ->
      context trackerName $ do

        describe "announce" $ do
          if tryAnnounce then do
            it "have valid response" $ do
              withManager rpcOpts $ \ mgr -> do
                q <- arbitrarySample
                announce mgr trackerURI q >>= validateInfo q
          else do
            it "should throw TimeoutExpired" $ do
              withManager rpcOpts $ \ mgr -> do
                q <- arbitrarySample
                announce mgr trackerURI q `shouldThrow` isTimeoutExpired

        describe "scrape" $ do
          if tryScraping then do
            it "have valid response" $ do
              withManager rpcOpts $ \ mgr -> do
                xs <- scrape mgr trackerURI [def]
                L.length xs `shouldSatisfy` (>= 1)
          else do
            it "should throw TimeoutExpired" $ do
              withManager rpcOpts $ \ mgr -> do
                scrape mgr trackerURI [def] `shouldThrow` isTimeoutExpired

        describe "Manager" $ do
          when tryScraping $ do
            it "should handle arbitrary intermixed concurrent queries" $ do
              withManager rpcOpts $ \ mgr -> do
                _ <- mapConcurrently (\ _ -> scrape mgr trackerURI [def]) [1..rpcCount]
                return ()