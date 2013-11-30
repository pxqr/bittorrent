{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker.RPC.UDPSpec (spec) where

import Control.Monad
import Data.Default
import Data.List as L
import Data.Maybe
import Network.URI
import Test.Hspec

import Network.BitTorrent.Tracker.RPC.MessageSpec hiding (spec)
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
          q <- arbitrarySample
          connect uri >>= announce q >>= validateInfo q

      describe "scrape" $ do
        it "have valid response" $ do
          xs <- connect uri >>= scrape [def]
          L.length xs `shouldSatisfy` (>= 1)
