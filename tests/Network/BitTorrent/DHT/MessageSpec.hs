{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.DHT.MessageSpec (spec) where
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Concurrent
import Data.BEncode as BE
import Data.ByteString.Lazy as BL
import Data.Default
import Data.List as L
import Data.Maybe
import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Message
import qualified Network.KRPC as KRPC (def)
import Network.KRPC hiding (def)
import Network.Socket (PortNumber)
import Test.Hspec
import Test.QuickCheck
import System.Timeout

import Data.TorrentSpec                 ()
import Network.BitTorrent.CoreSpec      ()
import Network.BitTorrent.DHT.TokenSpec ()


instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

remoteAddr :: SockAddr
remoteAddr = SockAddrInet 6881 (256 * 256 * 256 + 127)

thisAddr :: SockAddr
thisAddr = SockAddrInet 60000 (256 * 256 * 256 + 127)

thisPort :: PortNumber
thisPort = 60001

rpc :: ReaderT (Manager IO) IO a -> IO a
rpc action = do
  withManager KRPC.def thisAddr [] $ runReaderT $ do
    listen
    action

isQueryError :: QueryFailure -> Bool
isQueryError _ = True

prop_bencode :: Eq a => Show a => BEncode a => a -> Expectation
prop_bencode x = BE.decode (BL.toStrict (BE.encode x)) `shouldBe` Right x

retry :: Int -> IO (Maybe a) -> IO (Maybe a)
retry 0 _ = return Nothing
retry n a = do
  res <- a
  case res of
    Just _ -> return res
    Nothing -> threadDelay (100 * 1000) >> retry (n-1) a

spec :: Spec
spec = do
 context ("you need running DHT node at " ++ show remoteAddr) $ do
  it "is running" $ do
         running <- retry 5 $ timeout (100 * 1000) $ do
                      nid <- genNodeId
                      Response _remoteAddr Ping <-
                          rpc (query remoteAddr (Query nid Ping))
                      return ()
         running `shouldSatisfy` isJust

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

    it "properly bencoded (iso)" $ property $ \ nid -> do
      prop_bencode (Query    nid Ping)
      prop_bencode (Response nid Ping)

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

    it "properly bencoded (iso)" $ property $ \ nid x xs -> do
      prop_bencode (Query    nid (FindNode  x))
      prop_bencode (Response nid (NodeFound (xs :: [NodeInfo IPv4] )))

    it "does compatible with existing DHT" $ do
      nid <- genNodeId
      Response _remoteAddr (NodeFound xs) <- rpc $ do
        query remoteAddr (Query nid (FindNode nid))
      L.length (xs :: [NodeInfo IPv4]) `shouldSatisfy` (> 0)

  describe "get_peers" $ do
    it "properly bencoded" $ do
       BE.decode "d2:id20:abcdefghij0123456789\
                  \9:info_hash20:mnopqrstuvwxyz123456\
                  \e"
         `shouldBe` Right (Query "abcdefghij0123456789"
                           (GetPeers "mnopqrstuvwxyz123456"))

       BE.decode "d2:id20:abcdefghij0123456789\
                  \5:token8:aoeusnth\
                  \6:valuesl6:\127\0\0\1\1\2\&6:\192\168\1\100\1\2e\
                 \e"
         `shouldBe` Right (Response "abcdefghij0123456789"
             (GotPeers (Right [ "127.0.0.1:258" :: PeerAddr IPv4
                              , "192.168.1.100:258"
                              ]) "aoeusnth"))

       BE.decode "d2:id20:abcdefghij0123456789\
                  \5:nodes26:mnopqrstuvwxyz123456\127\0\0\1\1\2\
                  \5:token8:aoeusnth\
                 \e"
         `shouldBe` Right (Response "abcdefghij0123456789"
            (GotPeers
             { peers = Left [NodeInfo "mnopqrstuvwxyz123456" "127.0.0.1:258"
                                 :: NodeInfo IPv4]
             , grantedToken = "aoeusnth"
             }))

    it "properly bencoded (iso)" $ property $ \ nid topic exs token -> do
      prop_bencode (Query    nid (GetPeers topic))
      let _ = exs :: Either [NodeInfo IPv4] [PeerAddr IPv4]
      let nullPeerId paddr = paddr {peerId = Nothing}
      let nullPeerIds      = either Left (Right . L.map nullPeerId)
      prop_bencode (Response nid (GotPeers (nullPeerIds exs) token))

    it "does compatible with existing DHT" $ do
      nid <- genNodeId
      Response _remoteId (GotPeers {..})
        <- rpc $ query remoteAddr (Query nid (GetPeers def))
      let _ = peers :: Either [NodeInfo IPv4] [PeerAddr IPv4]
      either L.length L.length peers `shouldSatisfy` (> 0)

  describe "announce" $ do
    it "properly bencoded" $ do
      BE.decode "d2:id20:abcdefghij0123456789\
                 \9:info_hash20:mnopqrstuvwxyz123456\
                 \4:porti6881e\
                 \5:token8:aoeusnth\
                \e" `shouldBe` Right
        (Query "abcdefghij0123456789"
                   (Announce False "mnopqrstuvwxyz123456" 6881 "aoeusnth"))

      BE.decode "d2:id20:abcdefghij0123456789\
                 \12:implied_porti1e\
                 \9:info_hash20:mnopqrstuvwxyz123456\
                 \4:porti6881e\
                 \5:token8:aoeusnth\
                \e" `shouldBe` Right
        (Query "abcdefghij0123456789"
                   (Announce True "mnopqrstuvwxyz123456" 6881 "aoeusnth"))


      BE.decode "d2:id20:mnopqrstuvwxyz123456e"
        `shouldBe` Right
        (Response "mnopqrstuvwxyz123456" Announced)

    it "properly bencoded (iso)" $ property $ \ nid flag topic port token -> do
      prop_bencode (Query    nid (Announce flag topic port token))
      prop_bencode (Response nid (Announced))


    it "does compatible with existing DHT" $ do
      nid <- genNodeId
      Response _remoteId Announced <- rpc $ do
        Response _ GotPeers {..} <- query remoteAddr (Query nid (GetPeers def))
        let _ = peers :: Either [NodeInfo IPv4] [PeerAddr IPv4]
        query remoteAddr (Query nid (Announce False def thisPort grantedToken))
      return ()

    it "does fail on invalid token" $ do
      nid <- genNodeId
      (rpc $ do
        Response _ GotPeers {..} <- query remoteAddr (Query nid (GetPeers def))
        let _ = peers :: Either [NodeInfo IPv4] [PeerAddr IPv4]
        let invalidToken = ""
        query remoteAddr (Query nid (Announce False def thisPort invalidToken)))
          `shouldThrow` isQueryError
      return ()
