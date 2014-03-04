{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.DHT.QuerySpec (spec) where
import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Data.Conduit as C
import Data.Conduit.List as CL
import Data.Default
import Data.List as L
import Test.Hspec

import Network.BitTorrent.Core
import Network.BitTorrent.DHT
import Network.BitTorrent.DHT.Session
import Network.BitTorrent.DHT.Query

import Network.BitTorrent.DHT.TestData


myAddr :: NodeAddr IPv4
myAddr = "0.0.0.0:0"

nullLogger :: LogFun
nullLogger _ _ _ _ = return ()

--simpleLogger :: LogFun
--simpleLogger _ t _ _ = print t

simpleDHT :: [NodeHandler IPv4] -> DHT IPv4 a -> IO a
simpleDHT hs m =
  bracket (newNode hs def myAddr nullLogger) closeNode $ \ node ->
    runDHT node m

getBootInfo :: IO (NodeInfo IPv4)
getBootInfo = do
  startAddr <- resolveHostName (L.head defaultBootstrapNodes)
  simpleDHT [] $ pingQ startAddr

spec :: Spec
spec = parallel $ do
  describe "environment" $ do
    describe "test node" $ do
      it "is alive" $ do
         _ <- getBootInfo
         return ()

  describe "handlers" $ do
    it "" $ pendingWith "need to setup 2 DHT locally"

  describe "basic queries" $ do
    it "ping" $ do
      _ <- getBootInfo
      return ()

    it "findNode" $ do
      startInfo <- getBootInfo
      _ <- simpleDHT [] $ do
        nid <- asks thisNodeId
        findNodeQ nid startInfo
      return ()

    it "getPeers" $ do
      startInfo <- getBootInfo
      peers <- simpleDHT [] $ do
        nid <- asks thisNodeId

        -- we should not run getPeers query on boot node, because
        -- it may not support it
        Right infos <- findNodeQ nid startInfo

        when (L.null infos) $
          error "boot node malfunction"

        -- at least one node should reply
        queryParallel $ do
          getPeersQ (entryHash (L.head testTorrents)) <$> infos

      peers `shouldSatisfy` (not . L.null)

    it "announce" $ do
      bootNode <- getBootInfo
      _ <- simpleDHT [] $ do
        let ih = entryHash (L.head testTorrents)
        Right nodes <- findNodeQ ih bootNode

        when (L.null nodes) $
          error "boot node malfunction"

        queryParallel $ do
          announceQ ih (nodePort myAddr) <$> nodes

      return ()

  describe "iterative queries" $ do
    forM_ testTorrents $ \ TestEntry {..} -> do
      context entryName $ do

        it "get at least 10 unique peers for each infohash" $ do
          bootNode <- getBootInfo
          peers <- simpleDHT [] $ do
            Right startNodes <- findNodeQ entryHash bootNode
            sourceList [startNodes] $=
              search entryHash (getPeersQ entryHash) $=
                CL.concat $$ CL.take 10
          L.length peers `shouldBe` 10