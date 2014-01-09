{-# OPTIONS -fno-warn-orphans #-}
module Network.BitTorrent.Core.NodeInfoSpec (spec) where
import Control.Applicative
import Data.Serialize as S
import Data.String
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Core
import Network.BitTorrent.Core.PeerAddrSpec ()

instance Arbitrary NodeId where
  arbitrary = fromString <$> vector 20

instance Arbitrary a => Arbitrary (NodeAddr a) where
  arbitrary = NodeAddr <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (NodeInfo a) where
  arbitrary = NodeInfo <$> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "NodeId" $ do
    it "properly serialized" $ do
      S.decode "mnopqrstuvwxyz123456"
        `shouldBe` Right ("mnopqrstuvwxyz123456" :: NodeId)

      S.encode ("mnopqrstuvwxyz123456" :: NodeId)
        `shouldBe` "mnopqrstuvwxyz123456"

    it "properly serialized (iso)" $ property $ \ nid ->
      S.decode (S.encode nid) `shouldBe`
        Right (nid :: NodeId)

  describe "NodeAddr" $ do
    it "properly serialized" $ do
      S.decode "\127\0\0\1\1\2" `shouldBe`
        Right ("127.0.0.1:258" :: NodeAddr IPv4)

    it "properly serialized (iso)" $ property $ \ nid ->
      S.decode (S.encode nid) `shouldBe`
        Right (nid :: NodeAddr IPv4)

  describe "NodeInfo" $ do
    it "properly serialized" $ do
      S.decode "mnopqrstuvwxyz123456\
               \\127\0\0\1\1\2" `shouldBe` Right
       (NodeInfo "mnopqrstuvwxyz123456" "127.0.0.1:258" :: NodeInfo IPv4)

    it "properly serialized (iso)" $ property $ \ nid ->
      S.decode (S.encode nid) `shouldBe`
        Right (nid :: NodeInfo IPv4)
