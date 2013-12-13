module Network.BitTorrent.Tracker.RPCSpec (spec) where
import Control.Monad
import Data.Default
import Data.List as L
import Network.URI
import Test.Hspec

import Network.BitTorrent.Tracker.MessageSpec hiding (spec)
import Network.BitTorrent.Tracker.RPC.HTTPSpec as HTTP hiding (spec)
import Network.BitTorrent.Tracker.RPC.UDPSpec as UDP hiding (spec)
import Network.BitTorrent.Tracker.RPC as RPC

uris :: [URI]
uris = UDP.trackerURIs ++ HTTP.trackerURIs

spec :: Spec
spec = do
  forM_ uris $ \ uri ->
    context (show uri) $ do
      describe "announce" $ do
        it "have valid response" $ do
          q    <- arbitrarySample
          info <- connect uri >>= announce q
          validateInfo q info

      describe "scrape" $ do
        it "have valid response" $ do
          xs <- connect uri >>= scrape [def]
          L.length xs `shouldSatisfy` (>= 1)
