{-# LANGUAGE ScopedTypeVariables #-}
module Network.BitTorrent.DHT.SessionSpec (spec) where
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Default
import Data.List as L
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Address
import Network.BitTorrent.DHT
import Network.BitTorrent.DHT.Message
import Network.BitTorrent.DHT.Session

import Data.TorrentSpec ()
import Network.BitTorrent.CoreSpec ()
import Network.BitTorrent.DHT.TokenSpec ()


myAddr :: NodeAddr IPv4
myAddr = "127.0.0.1:60000"

simpleDHT :: DHT IPv4 a -> IO a
simpleDHT m =
  bracket (newNode defaultHandlers def myAddr nullLogger) closeNode $ \ node ->
    runDHT node m

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

isLeft :: Either a b -> Bool
isLeft = not . isRight

nullLogger :: LogFun
nullLogger _ _ _ _ = return ()

spec :: Spec
spec = do
  describe "session" $ do
    it "is active until stopNode called" $ do
      node <- newNode [] def myAddr nullLogger
      runDHT node monadActive `shouldReturn` True
      runDHT node monadActive `shouldReturn` True
      closeNode node
      runDHT node monadActive `shouldReturn` False

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

  describe "routing table" $
    it "accept any node entry when table is empty" $
      property $ \ (nid :: NodeId) -> do
        let info = NodeInfo nid myAddr
        closest <- simpleDHT $ do
           _ <- insertNode info
           liftIO $ yield
           getClosest nid
        closest `shouldSatisfy` L.elem info

  describe "peer storage" $ do
    it "should return nodes, if there are no peers" $ property $ \ ih -> do
      res <- simpleDHT $ do getPeerList ih
      res `shouldSatisfy` isLeft

    it "should return peers, if any" $ property $ \ ih addr -> do
      res <- simpleDHT $ do
                 insertPeer ih addr
                 getPeerList ih
      res `shouldSatisfy` isRight

  describe "topic storage" $ do
    it "should not grow indefinitely" $ do
      pending

  describe "messaging" $ do
    describe "queryNode" $ do
      it "should always ping this node" $ do
        (rid, tid) <- simpleDHT $ do
          (remoteId, Ping) <- queryNode myAddr Ping
          thisId    <-  asks thisNodeId
          return (remoteId, thisId)
        rid `shouldBe` tid

    describe "queryParallel" $ do
      it "should handle parallel requests" $ do
        (nid, resps) <- simpleDHT $ (,)
          <$> asks thisNodeId
          <*> queryParallel (L.replicate 100 $ queryNode myAddr Ping)
        resps `shouldSatisfy` L.all (== (nid, Ping))

    describe "(<@>) operator" $ do
      it "" $
         pending