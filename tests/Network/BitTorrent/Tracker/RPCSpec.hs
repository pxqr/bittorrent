module Network.BitTorrent.Tracker.RPCSpec (spec) where
import Control.Applicative
import Control.Monad
import Data.Default
import Data.List as L
import Network.URI
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Tracker.MessageSpec hiding (spec)
import Network.BitTorrent.Tracker.RPC.HTTPSpec as HTTP hiding (spec)
import Network.BitTorrent.Tracker.RPC.UDPSpec as UDP hiding (spec)
import Network.BitTorrent.Tracker.RPC as RPC

uris :: [URI]
uris = UDP.trackerURIs ++ HTTP.trackerURIs

pinfo :: PeerInfo
pinfo = PeerInfo "-HS0003-203534.37422" 6000 Nothing

instance Arbitrary SAnnounceQuery where
  arbitrary = SAnnounceQuery <$> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
  forM_ uris $ \ uri ->
    context (show uri) $ do
      describe "announce" $ do
        it "have valid response" $ do
          withManager def pinfo $ \ mgr -> do
            q    <- arbitrarySample
            info <- announce mgr uri q
            return ()

      describe "scrape" $ do
        it "have valid response" $ do
          withManager def pinfo $ \ mgr -> do
            xs <- scrape mgr uri [def]
            L.length xs `shouldSatisfy` (>= 1)
