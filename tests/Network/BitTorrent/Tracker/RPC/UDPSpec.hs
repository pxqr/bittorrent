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


trackerURIs :: [URI]
trackerURIs =
  [ fromJust $ parseURI "udp://tracker.openbittorrent.com:80/announce"
  , fromJust $ parseURI "udp://tracker.publicbt.com:80/announce"
  ]

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
