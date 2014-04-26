{-# LANGUAGE ScopedTypeVariables #-}
module Network.BitTorrent.DHT.RoutingSpec (spec) where
import Control.Applicative
import Control.Monad.State
import Data.Default
import Data.List as L
import Data.Maybe
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Address
import Network.BitTorrent.DHT.Routing as T

import Network.BitTorrent.CoreSpec hiding (spec)


type Network ip = [NodeAddr ip]

data Env ip = Env
  { currentTime :: Timestamp
  , network     :: Network ip
  } deriving Show

type Simulation ip = State (Env ip)

runSimulation :: Eq ip => Env ip -> Routing ip a -> Maybe a
runSimulation e m = evalState (runRouting ping closest timestamp m) e
  where
    ping    addr = gets (L.elem addr . network)
    closest nid  = error "runSimulation"
    timestamp    = gets currentTime

instance Arbitrary ip => Arbitrary (Env ip) where
  arbitrary = Env <$> arbitrary <*> (vector nodeCount)
    where
      nodeCount = 1000

instance (Arbitrary ip, Eq ip) => Arbitrary (Table ip) where
  arbitrary = do
    thisId  <- arbitrary
    bucketN <- choose (1, 20)
    let table = nullTable thisId bucketN

--    nodeN   <- (`mod` bucketN) <$> arbitrary
--    nodes   <- vector nodeN

    node    <- arbitrary
    mt <- runSimulation  <$> arbitrary
          <*> pure (T.insert node table)
          --(foldM (flip fillTable) table nodes)
    return (fromJust mt)
   where
    fillTable x t = do
      t' <- T.insert x t
      return $ if T.full t' then t else t'

spec :: Spec
spec = do
  describe "size" $ do
    it "null table is empty" $ do
      T.size (nullTable def 2 :: Table IPv4) `shouldBe` 0

    it "the same node never appear in different buckets" $ property $ \ t -> do
      let xss = T.toList (t :: Table Int)
      let justOnce x = L.length (L.filter (L.elem x) xss) == 1
      L.all justOnce (L.concat xss)

    it "insert is idemponent" $ do
      pending
{-
  property $ \ (e :: Env Int) n t -> do

      let t1 = runSimulation e (T.insert n t)
      let t2 = runSimulation e (T.insert n t >>= T.insert n)
      t1 `shouldBe` t2
-}