{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.BitTorrent.Tracker.RPCSpec (spec) where
import Control.Applicative
import Control.Monad
import Data.Default
import Data.List as L
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Tracker.RPC as RPC

import           Network.BitTorrent.Tracker.TestData
import           Network.BitTorrent.Tracker.MessageSpec hiding (spec)
import qualified Network.BitTorrent.Tracker.RPC.UDPSpec as UDP (rpcOpts)


instance Arbitrary SAnnounceQuery where
  arbitrary = SAnnounceQuery <$> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary

rpcOpts :: Options
rpcOpts = def
  { optUdpRPC = UDP.rpcOpts
  }

isUnrecognizedProtocol :: String -> RpcException -> Bool
isUnrecognizedProtocol x (UnrecognizedProtocol scheme) = x == scheme
isUnrecognizedProtocol _  _                            = False

spec :: Spec
spec = parallel $ do
  describe "Manager" $ do
    describe "newManager" $ do
      it "" $ pending

    describe "closeManager" $ do
      it "" $ pending

    describe "withManager" $ do
      it "" $ pending

  describe "RPC" $ do
    describe "announce" $ do
      it "must fail on bad uri scheme" $ do
        withManager rpcOpts def $ \ mgr -> do
          q    <- arbitrarySample
          announce mgr "magnet://foo.bar" q
            `shouldThrow` isUnrecognizedProtocol "magnet:"

    describe "scrape" $ do
      it "must fail on bad uri scheme" $ do
        withManager rpcOpts def $ \ mgr -> do
          scrape mgr "magnet://foo.bar" []
            `shouldThrow` isUnrecognizedProtocol "magnet:"

    forM_ trackers $ \ TrackerEntry {..} ->
      context trackerName $ do

        describe "announce" $ do
          if tryAnnounce then do
            it "have valid response" $ do
              withManager rpcOpts def $ \ mgr -> do
                q    <- arbitrarySample
                _    <- announce mgr trackerURI q
                return ()
          else do
            it "should throw exception" $ do
              pending

        describe "scrape" $ do
          if tryScraping then do
            it "have valid response" $ do
              withManager rpcOpts def $ \ mgr -> do
                xs <- scrape mgr trackerURI [def]
                L.length xs `shouldSatisfy` (>= 1)
          else do
            it "should throw exception" $ do
              pending
