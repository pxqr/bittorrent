{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.BitTorrent.Core.PeerAddrSpec (spec) where
import Control.Applicative
import Data.BEncode as BE
import Data.ByteString.Lazy as BL
import Data.IP
import Data.Serialize as S
import Data.Word
import Network
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Core.PeerIdSpec hiding (spec)
import Network.BitTorrent.Core.PeerAddr

instance Arbitrary IPv4 where
  arbitrary = do
    a <- choose (0, 255)
    b <- choose (0, 255)
    c <- choose (0, 255)
    d <- choose (0, 255)
    return $ toIPv4 [a, b, c, d]

instance Arbitrary IPv6 where
  arbitrary = do
    a <- choose (0, fromIntegral (maxBound :: Word16))
    b <- choose (0, fromIntegral (maxBound :: Word16))
    c <- choose (0, fromIntegral (maxBound :: Word16))
    d <- choose (0, fromIntegral (maxBound :: Word16))
    e <- choose (0, fromIntegral (maxBound :: Word16))
    f <- choose (0, fromIntegral (maxBound :: Word16))
    g <- choose (0, fromIntegral (maxBound :: Word16))
    h <- choose (0, fromIntegral (maxBound :: Word16))
    return $ toIPv6 [a, b, c, d, e, f, g, h]

instance Arbitrary IP where
  arbitrary = frequency
    [ (1, IPv4 <$> arbitrary)
    , (1, IPv6 <$> arbitrary)
    ]

instance Arbitrary PortNumber where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word16)

instance Arbitrary a => Arbitrary (PeerAddr a) where
  arbitrary = PeerAddr <$> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "PortNumber" $ do
    it "properly serialized" $ do
      S.decode "\x1\x2" `shouldBe` Right (258 :: PortNumber)
      S.encode (258 :: PortNumber) `shouldBe` "\x1\x2"

    it "properly bencoded" $ do
      BE.decode "i80e" `shouldBe` Right (80 :: PortNumber)

    it "fail if port number is invalid" $ do
      (BE.decode "i-10e" :: BE.Result PortNumber)
        `shouldBe`
        Left "fromBEncode: unable to decode PortNumber: -10"

      (BE.decode "i70000e" :: BE.Result PortNumber)
        `shouldBe`
        Left "fromBEncode: unable to decode PortNumber: 70000"

  describe "Peer IPv4" $ do
    it "properly serialized" $ do
      S.decode "\x1\x2\x3\x4" `shouldBe` Right (toIPv4 [1, 2, 3, 4])
      S.encode (toIPv4 [1, 2, 3, 4]) `shouldBe` "\x1\x2\x3\x4"

    it "properly serialized (iso)" $ property $ \ ip -> do
      S.decode (S.encode ip) `shouldBe` Right (ip :: IPv4)

    it "properly bencoded" $ do
      BE.decode "11:168.192.0.1" `shouldBe` Right (toIPv4 [168, 192, 0, 1])
      BE.encode (toIPv4 [168, 192, 0, 1]) `shouldBe` "11:168.192.0.1"

    it "properly bencoded (iso)" $ property $ \ ip ->
      BE.decode (BL.toStrict (BE.encode ip)) `shouldBe` Right (ip :: IPv4)

    it "fail gracefully on invalid strings" $ do
      BE.decode "3:1.1" `shouldBe`
        (Left "fromBEncode: unable to decode IP: 1.1" :: BE.Result IPv4)

    it "fail gracefully on invalid bencode" $ do
      BE.decode "i10e" `shouldBe`
        (Left "fromBEncode: unable to decode IP: addr should be a bstring"
         :: BE.Result IPv4)

  describe "Peer IPv6" $ do
    it "properly serialized" $ do
      S.decode "\x1\x2\x3\x4\x5\x6\x7\x8\x9\xa\xb\xc\xd\xe\xf\x10"
        `shouldBe`
        Right ("102:304:506:708:90a:b0c:d0e:f10" :: IPv6)

      S.encode ("102:304:506:708:90a:b0c:d0e:f10" :: IPv6)
        `shouldBe`
        "\x1\x2\x3\x4\x5\x6\x7\x8\x9\xa\xb\xc\xd\xe\xf\x10"

    it "properly serialized (iso)" $ property $ \ ip ->
      S.decode (S.encode ip) `shouldBe` Right (ip :: IPv6)

    it "properly bencoded" $ do
      BE.decode "3:::1" `shouldBe` Right (toIPv6 [0, 0, 0, 0, 0, 0, 0, 1])
      BE.encode (toIPv6 [0, 0, 0, 0, 0, 0, 0, 1]) `shouldBe`
        "23:00:00:00:00:00:00:00:01"

      BE.decode "23:00:00:00:00:00:00:00:01"
        `shouldBe`
        Right (toIPv6 [0, 0, 0, 0, 0, 0, 0, 1])

    it "properly bencoded iso" $ property $ \ ip ->
      BE.decode (BL.toStrict (BE.encode ip)) `shouldBe` Right (ip :: IPv4)

    it "fail gracefully on invalid strings" $ do
      BE.decode "4:g::1" `shouldBe`
        (Left "fromBEncode: unable to decode IP: g::1" :: BE.Result IPv6)

    it "fail gracefully on invalid bencode" $ do
      BE.decode "i10e" `shouldBe`
        (Left "fromBEncode: unable to decode IP: addr should be a bstring"
         :: BE.Result IPv6)


  describe "Peer IP" $ do
    it "properly serialized IPv6" $ do
      S.decode "\x1\x2\x3\x4\x5\x6\x7\x8\x9\xa\xb\xc\xd\xe\xf\x10"
        `shouldBe`
        Right ("102:304:506:708:90a:b0c:d0e:f10" :: IP)

      S.encode ("102:304:506:708:90a:b0c:d0e:f10" :: IP)
        `shouldBe`
        "\x1\x2\x3\x4\x5\x6\x7\x8\x9\xa\xb\xc\xd\xe\xf\x10"

    it "properly serialized (iso) IPv6" $ property $ \ ip ->
      S.decode (S.encode ip) `shouldBe` Right (ip :: IP)

    it "properly serialized IPv4" $ do
      S.decode "\x1\x2\x3\x4" `shouldBe` Right (IPv4 $ toIPv4 [1, 2, 3, 4])
      S.encode (toIPv4 [1, 2, 3, 4]) `shouldBe` "\x1\x2\x3\x4"

    it "properly serialized (iso) IPv4" $ property $ \ ip -> do
      S.decode (S.encode ip) `shouldBe` Right (ip :: IP)

    it "properly bencoded" $ do
      BE.decode "11:168.192.0.1" `shouldBe`
        Right (IPv4 (toIPv4 [168, 192, 0, 1]))

      BE.decode "3:::1" `shouldBe` Right
         (IPv6 (toIPv6 [0, 0, 0, 0, 0, 0, 0, 1]))

      BE.encode (IPv6 (toIPv6 [0, 0, 0, 0, 0, 0, 0, 1])) `shouldBe`
        "23:00:00:00:00:00:00:00:01"

      BE.decode "23:00:00:00:00:00:00:00:01"
        `shouldBe`
        Right (IPv6 (toIPv6 [0, 0, 0, 0, 0, 0, 0, 1]))

    it "properly bencoded iso" $ property $ \ ip ->
      BE.decode (BL.toStrict (BE.encode ip)) `shouldBe` Right (ip :: IP)

    it "fail gracefully on invalid strings" $ do
      BE.decode "4:g::1" `shouldBe`
        (Left "fromBEncode: unable to decode IP: g::1" :: BE.Result IP)

    it "fail gracefully on invalid bencode" $ do
      BE.decode "i10e" `shouldBe`
        (Left "fromBEncode: unable to decode IP: addr should be a bstring"
         :: BE.Result IP)

  describe "PeerAddr" $ do
    it "IsString" $ do
      ("127.0.0.1:80" :: PeerAddr IP)
        `shouldBe` PeerAddr Nothing "127.0.0.1" 80

      ("127.0.0.1:80" :: PeerAddr IPv4)
        `shouldBe` PeerAddr Nothing "127.0.0.1" 80

      ("[::1]:80"     :: PeerAddr IP)
        `shouldBe` PeerAddr Nothing "::1"       80

      ("[::1]:80"     :: PeerAddr IPv6)
        `shouldBe` PeerAddr Nothing "::1"       80

    it "properly bencoded (iso)" $ property $ \ addr ->
      BE.decode (BL.toStrict (BE.encode addr))
        `shouldBe` Right (addr :: PeerAddr IP)


    it "properly bencoded (ipv4)" $ do
      BE.decode "d2:ip11:168.192.0.1\
                 \7:peer id20:01234567890123456789\
                 \4:porti6881e\
                 \e"
        `shouldBe`
        Right (PeerAddr (Just "01234567890123456789")
                        (IPv4 (toIPv4 [168, 192, 0, 1]))
                        6881)

    it "properly bencoded (ipv6)" $ do
      BE.decode "d2:ip3:::1\
                 \7:peer id20:01234567890123456789\
                 \4:porti6881e\
                 \e"
        `shouldBe`
        Right (PeerAddr (Just "01234567890123456789")
                        (IPv6 (toIPv6 [0, 0, 0, 0, 0, 0, 0, 1]))
                        6881)

    it "peer id is optional" $ do
      BE.decode "d2:ip11:168.192.0.1\
                 \4:porti6881e\
                 \e"
        `shouldBe`
        Right (PeerAddr Nothing (IPv4 (toIPv4 [168, 192, 0, 1])) 6881)

    it "has sock addr for both ipv4 and ipv6" $ do
      show (peerSockAddr "128.0.0.1:80") `shouldBe` "128.0.0.1:80"
      show (peerSockAddr "[::1]:8080"  ) `shouldBe` "[::1]:8080"
