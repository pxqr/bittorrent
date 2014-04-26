module Network.BitTorrent.DHTSpec (spec) where
import Control.Exception
import Control.Monad
import Data.Default
import Data.List as L
import Test.Hspec
import System.Timeout

import Data.Torrent
import Network.BitTorrent.DHT


partialBootstrapTimeout :: Int
partialBootstrapTimeout = 10 * 1000000

opts :: Options
opts = def { optBucketCount = 1 }

-- NOTE to shorten test cases run time include only "good" infohashes
-- with many nodes
existingInfoHashes :: [InfoHash]
existingInfoHashes =
  [
  ]

-- TODO use Test.Hspec.parallel

spec :: Spec
spec = do
  describe "bootstrapping" $ do
    it "should resolve all default bootstrap nodes" $ do
      nodes <- forM defaultBootstrapNodes resolveHostName
      _     <- evaluate nodes
      return ()

    it "partial bootstrapping should finish in less than 10 seconds" $ do
      node <- resolveHostName (L.head defaultBootstrapNodes)
      res  <- timeout partialBootstrapTimeout $ do
                dht opts def $ do
                  bootstrap [node]
                  isBootstrapped
      res `shouldBe` Just True

  describe "initialization" $ do
    it "should be bootstrapped after restore process" $ do
      pending

  describe "lookup" $ do
    describe "for any existing infohash" $ do
      forM_ existingInfoHashes $ \ ih -> do
        context (show ih) $ do
          it "should find peers" $ do
            pending

  describe "insert" $ do
    it "should return this peer if announced" $ do
      pending

  describe "delete" $ do
    return ()