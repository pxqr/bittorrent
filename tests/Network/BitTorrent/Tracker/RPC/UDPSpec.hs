{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker.RPC.UDPSpec (spec, trackerURIs) where

import Control.Monad
import Data.Default
import Data.List as L
import Data.Maybe
import Network.URI
import Test.Hspec

import Network.BitTorrent.Tracker.MessageSpec hiding (spec)
import Network.BitTorrent.Tracker.RPC.UDP

import Network.BitTorrent.Core
import Network.BitTorrent.Tracker.Message as Message


trackerURIs :: [URI]
trackerURIs =
  [ fromJust $ parseURI "udp://tracker.openbittorrent.com:80/announce"
  , fromJust $ parseURI "udp://tracker.publicbt.com:80/announce"
  ]

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

spec :: Spec
spec = do
  forM_ trackerURIs $ \ uri ->
    context (show uri) $ do
      describe "announce" $ do
        it "have valid response" $ do
          withManager def $ \ mgr -> do
            q <- arbitrarySample
            announce mgr uri q >>= validateInfo q

      describe "scrape" $ do
        it "have valid response" $ do
          withManager def $ \ mgr -> do
            xs <- scrape mgr uri [def]
            L.length xs `shouldSatisfy` (>= 1)
