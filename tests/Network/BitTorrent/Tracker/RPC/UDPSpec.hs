{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker.RPC.UDPSpec (spec) where

import Control.Applicative
import Control.Monad
import Data.Default
import Data.List as L
import Data.Maybe
import Network.URI
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Core.PeerAddr
import Network.BitTorrent.Tracker.RPC.Message
import Network.BitTorrent.Tracker.RPC.UDP
import Network.BitTorrent.Tracker.RPC.MessageSpec ()


arbitrarySample :: Arbitrary a => IO a
arbitrarySample = L.head <$> sample' arbitrary

trackerURIs :: [URI]
trackerURIs =
  [ fromJust $ parseURI "udp://tracker.openbittorrent.com:80/announce"
  , fromJust $ parseURI "udp://tracker.publicbt.com:80/announce"
  ]

-- relation with query: peer id, numwant
validateInfo :: AnnounceQuery -> AnnounceInfo -> Expectation
validateInfo AnnounceQuery {..}  AnnounceInfo {..} = do
    respComplete    `shouldSatisfy` isJust
    respIncomplete  `shouldSatisfy` isJust
    respMinInterval `shouldSatisfy` isNothing
    respWarning     `shouldSatisfy` isNothing
    peerList `shouldSatisfy` L.all (isNothing . peerID)
    fromJust respComplete + fromJust respIncomplete `shouldBe` L.length peerList
  where
    peerList = getPeerList respPeers


spec :: Spec
spec = do
  forM_ trackerURIs $ \ uri ->
    context (show uri) $ do
      describe "announce" $ do
        it "have valid response" $ do
          query <- arbitrarySample
          connect uri >>= announce query >>= validateInfo query

      describe "scrape" $ do
        it "have valid response" $ do
          xs <- connect uri >>= scrape [def]
          return ()
--          L.length xs `shouldSatisfy` (>= 1)
