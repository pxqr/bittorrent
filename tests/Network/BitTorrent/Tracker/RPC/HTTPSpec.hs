module Network.BitTorrent.Tracker.RPC.HTTPSpec (spec) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Default
import Data.List as L
import Data.Maybe
import Network.URI
import Test.Hspec

import Network.BitTorrent.Tracker.MessageSpec hiding (spec)
import Network.BitTorrent.Tracker.RPC.HTTP


trackerURIs :: [URI]
trackerURIs =
  [ fromJust $ parseURI "http://announce.opensharing.org:2710/announce"
  , fromJust $ parseURI "http://exodus.desync.com/announce"
  ]

spec :: Spec
spec = do
  forM_ trackerURIs $ \ uri ->
    context (show uri) $ do
      describe "announce" $ do
        it "have valid response" $ do
          q    <- arbitrarySample
          info <- runResourceT $ connect uri >>= announce q
          validateInfo q info

      describe "scrape" $ do
        it "have valid response" $ do
          xs <- runResourceT $ connect uri >>= scrape [def]
          L.length xs `shouldSatisfy` (>= 1)
