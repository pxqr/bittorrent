module Network.BitTorrent.DHT.MessageSpec (spec) where
import Test.Hspec
import Data.BEncode as BE
import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Message


spec :: Spec
spec = do
  describe "ping" $ do
    it "properly bencoded" $ do
      BE.decode "d2:id20:abcdefghij0123456789e"
        `shouldBe` Right (Query "abcdefghij0123456789" Ping)

      BE.encode (Query "abcdefghij0123456789" Ping)
        `shouldBe` "d2:id20:abcdefghij0123456789e"

      BE.decode "d2:id20:mnopqrstuvwxyz123456e"
        `shouldBe` Right (Response "mnopqrstuvwxyz123456" Ping)

      BE.encode (Response "mnopqrstuvwxyz123456" Ping)
        `shouldBe` "d2:id20:mnopqrstuvwxyz123456e"

  describe "find_node" $ do
    it "properly bencoded" $ do
      BE.decode "d2:id20:abcdefghij0123456789\
                 \6:target20:mnopqrstuvwxyz123456e"
        `shouldBe` Right (Query "abcdefghij0123456789"
                      (FindNode "mnopqrstuvwxyz123456"))

      BE.encode (Query "abcdefghij0123456789"
                 (FindNode "mnopqrstuvwxyz123456"))
        `shouldBe`
         "d2:id20:abcdefghij01234567896:target20:mnopqrstuvwxyz123456e"

      let addr = "127.0.0.1:256" :: NodeAddr IPv4
      let nid  = "0123456789abcdefghij"
      BE.decode "d2:id20:0123456789abcdefghij\
                 \5:nodes26:mnopqrstuvwxyz123456\127\0\0\1\&56\
                 \e"
       `shouldBe` Right (Response nid (NodeFound [NodeInfo nid addr]))