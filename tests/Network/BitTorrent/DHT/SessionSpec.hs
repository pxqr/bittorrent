{-# LANGUAGE ScopedTypeVariables #-}
module Network.BitTorrent.DHT.SessionSpec (spec) where
import Control.Monad.Reader
import Data.Default
import Data.List as L
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Core
import Network.BitTorrent.DHT
import Network.BitTorrent.DHT.Message
import Network.BitTorrent.DHT.Session

import Data.Torrent.InfoHashSpec ()
import Network.BitTorrent.CoreSpec ()
import Network.BitTorrent.DHT.TokenSpec ()


myAddr :: NodeAddr IPv4
myAddr = "127.0.0.1:60000"

simpleDHT :: DHT IPv4 a -> IO a
simpleDHT = dht def myAddr

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

isLeft :: Either a b -> Bool
isLeft = not . isRight

spec :: Spec
spec = do
  describe "tokens" $ do
    it "should not complain about valid token" $
      property $ \ (addrs :: [NodeAddr IPv4]) -> do
        isOks <- simpleDHT $ do
          forM addrs $ \ addr -> do
            token <- grantToken addr
            checkToken addr token
        L.and isOks `shouldBe` True

    it "should complain about invalid token" $
      property $ \ (addr :: NodeAddr IPv4) token -> do
        isOk <- simpleDHT (checkToken addr token)
        isOk `shouldBe` False

  describe "routing table" $ do
    return ()

  describe "peer storage" $ do
    it "should return nodes, if there are no peers" $ property $ \ ih -> do
      nodes <- simpleDHT $ do getPeerList ih
      nodes `shouldSatisfy` isLeft

    it "should return peers, if any" $ property $ \ ih addr -> do
      peers <- simpleDHT $ do
                 insertPeer ih addr
                 getPeerList ih
      peers `shouldSatisfy` isRight

  describe "topic storage" $ do
    return ()

  describe "messaging" $ do
    describe "queryNode" $ do
      it "should always ping this node" $ do
        (rid, tid) <- simpleDHT $ do
          (remoteId, Ping) <- queryNode myAddr Ping
          thisId    <-  asks thisNodeId
          return (remoteId, thisId)
        rid `shouldBe` tid

    describe "queryParallel" $ do
      return ()

    describe "(<@>) operator" $ do
      return ()