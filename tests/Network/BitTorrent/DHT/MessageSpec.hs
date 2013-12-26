{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.DHT.MessageSpec (spec) where
import Control.Monad.Reader
import Test.Hspec
import Data.BEncode as BE
import Data.Default
import Data.List as L
import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Message
import Network.KRPC
import Network.Socket (PortNumber)


remoteAddr :: SockAddr
remoteAddr = SockAddrInet 6881 (256 * 256 * 256 + 127)

thisAddr :: SockAddr
thisAddr = SockAddrInet 60000 (256 * 256 * 256 + 127)

thisPort :: PortNumber
thisPort = 60001

rpc :: ReaderT (Manager IO) IO a -> IO a
rpc action = do
  withManager thisAddr [] $ runReaderT $ do
    listen
    action

isProtocolError :: KError -> Bool
isProtocolError KError {..} = errorCode == ProtocolError

spec :: Spec
spec = do
 context ("you need running DHT node at " ++ show remoteAddr) $ do
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

    it "properly bencoded (iso)" $ do
      pending

    it "does compatible with existing DHT" $ do
      nid <- genNodeId
      Response _remoteAddr Ping <- rpc (query remoteAddr (Query nid Ping))
      return ()

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

      let naddr = "127.0.0.1:258" :: NodeAddr IPv4
      let nid  = "0123456789abcdefghij"
      let nid' = "mnopqrstuvwxyz123456"
      BE.decode "d2:id20:0123456789abcdefghij\
                 \5:nodes26:mnopqrstuvwxyz123456\127\0\0\1\1\2\
                \e"
       `shouldBe` Right (Response nid (NodeFound [NodeInfo nid' naddr]))

    it "properly bencoded (iso)" $ do
      pending

    it "does compatible with existing DHT" $ do
      nid <- genNodeId
      Response _remoteAddr (NodeFound xs) <- rpc $ do
        query remoteAddr (Query nid (FindNode nid))
      L.length (xs :: [NodeInfo IPv4]) `shouldSatisfy` (> 0)

  describe "get_peers" $ do
    it "properly bencoded" $ do
      pending

    it "properly bencoded (iso)" $ do
      pending

    it "does compatible with existing DHT" $ do
      nid <- genNodeId
      Response _remoteId (GotPeers {..})
        <- rpc $ query remoteAddr (Query nid (GetPeers def))
      let _ = peers :: Either [NodeInfo IPv4] [PeerAddr IPv4]
      either L.length L.length peers `shouldSatisfy` (> 0)

  describe "announce" $ do
    it "properly bencoded" $ do
      pending

    it "properly bencoded (iso)" $ do
      pending

    it "does compatible with existing DHT" $ do
      nid <- genNodeId
      Response _remoteId Announced <- rpc $ do
        Response _ GotPeers {..} <- query remoteAddr (Query nid (GetPeers def))
        let _ = peers :: Either [NodeInfo IPv4] [PeerAddr IPv4]
        query remoteAddr (Query nid (Announce def thisPort grantedToken))
      return ()

    it "does fail on invalid token" $ do
      nid <- genNodeId
      (rpc $ do
        Response _ GotPeers {..} <- query remoteAddr (Query nid (GetPeers def))
        let _ = peers :: Either [NodeInfo IPv4] [PeerAddr IPv4]
        let invalidToken = ""
        query remoteAddr (Query nid (Announce def thisPort invalidToken)))
          `shouldThrow` isProtocolError
      return ()
