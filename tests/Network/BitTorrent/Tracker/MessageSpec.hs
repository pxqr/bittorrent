{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans  #-}
module Network.BitTorrent.Tracker.MessageSpec
       ( spec
       , arbitrarySample
       ) where

import Control.Applicative
import Control.Exception
import Data.BEncode as BE
import Data.ByteString.Lazy as BL
import Data.List as L
import Data.Maybe
import Test.Hspec
import Test.QuickCheck

import Data.TorrentSpec ()
import Data.Torrent.ProgressSpec ()
import Network.BitTorrent.Address ()
import Network.BitTorrent.Address ()

import Network.BitTorrent.Tracker.Message as Message
import Network.BitTorrent.Address


--prop_bencode :: Eq a => BEncode a => a -> Bool
--prop_bencode a = BE.decode (BL.toStrict (BE.encode a)) == return a

--prop_urlencode :: Eq a => URLDecoded a => URLEncoded a => a -> Bool
--prop_urlencode a = urlDecode (T.pack (urlEncode a)) == a

instance Arbitrary AnnounceEvent where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary AnnounceQuery where
  arbitrary = AnnounceQuery
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (PeerList IP) where
  arbitrary = frequency
    [ (1,        (PeerList . maybeToList) <$> arbitrary)
    , (1, (CompactPeerList . maybeToList . fmap zeroPeerId) <$> arbitrary)
    ]

  shrink (       PeerList xs) =        PeerList <$> shrink xs
  shrink (CompactPeerList xs) = CompactPeerList <$> shrink xs

instance Arbitrary AnnounceInfo where
  arbitrary = AnnounceInfo
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary

arbitrarySample :: Arbitrary a => IO a
arbitrarySample = L.head <$> sample' arbitrary

zeroPeerId :: PeerAddr a -> PeerAddr a
zeroPeerId addr = addr { peerId = Nothing }

spec :: Spec
spec = do
  describe "AnnounceQuery" $ do
    it "properly url encoded" $ property $ \ q ->
      parseAnnounceQuery (renderAnnounceQuery q)
        `shouldBe` Right q

  describe "PeerList" $ do
    context "Non compact" $ do
      it "properly encoded (both ipv4 and ipv6)" $ do
        BE.decode "ld2:ip7:1.2.3.44:porti80eed2:ip3:::14:porti8080eee"
          `shouldBe` Right
          (PeerList ["1.2.3.4:80", "[::1]:8080"] :: PeerList IPv4)

      it "properly encoded (iso)" $ property $ \ xs ->
        BE.decode (BL.toStrict (BE.encode (PeerList xs :: PeerList IPv4)))
          `shouldBe` Right (PeerList xs :: PeerList IPv4)

    context "Compact" $ do
      it "properly encodes (ipv4)" $ do
        BE.decode "12:\x1\x2\x3\x4\x1\x2\x9\x8\x7\x6\x1\x2"
          `shouldBe` Right
          (CompactPeerList ["1.2.3.4:258", "9.8.7.6:258"] :: PeerList IPv4)

      it "properly encodes (ipv6)" $ do
        BE.decode "18:\x1\x2\x3\x4\x5\x6\x7\x8\x1\x2\x3\x4\x5\x6\x7\x8\x1\x2"
          `shouldBe` Right
          (CompactPeerList ["[102:304:506:708:102:304:506:708]:258"]
           :: PeerList IPv6)

      it "properly encoded (ipv4, iso)" $
        property $ \ (fmap zeroPeerId -> xs) ->
          BE.decode (BL.toStrict (BE.encode (CompactPeerList xs)))
            `shouldBe` Right (CompactPeerList xs :: PeerList IPv4)

      it "properly encoded (ipv6, iso)" $
        property $ \ (fmap zeroPeerId -> xs) ->
          BE.decode (BL.toStrict (BE.encode (CompactPeerList xs)))
            `shouldBe` Right (CompactPeerList xs :: PeerList IPv6)

  describe "AnnounceInfo" $ do
    it "parses minimal sample" $ do
      "d8:intervali0e5:peerslee"
        `shouldBe`
        AnnounceInfo Nothing Nothing 0 Nothing (PeerList []) Nothing

    it "parses optional fields" $ do
      "d8:completei1e\
       \10:incompletei2e\
       \8:intervali3e\
       \12:min intervali4e\
       \5:peersle\
       \15:warning message3:str\
       \e"
        `shouldBe`
        AnnounceInfo (Just 1) (Just 2) 3 (Just 4) (PeerList []) (Just "str")

    it "parses failed response" $ do
      "d14:failure reason10:any reasone"
                 `shouldBe`
        Message.Failure "any reason"

    it "fail if no peer list present" $ do
      evaluate ("d8:intervali0ee" :: AnnounceInfo)
        `shouldThrow`
         errorCall "fromString: unable to decode AnnounceInfo: \
                   \required field `peers' not found"

    it "parses `peer' list" $ do -- TODO
      "d8:intervali0e\
       \5:peersl\
               \d2:ip7:1.2.3.4\
                \4:porti80e\
               \e\
               \d2:ip3:::1\
                \4:porti80e\
               \e\
              \e\
       \e" `shouldBe`
        let xs = PeerList ["1.2.3.4:80", "[::1]:80"] in
        AnnounceInfo Nothing Nothing 0 Nothing xs Nothing

    it "parses `peers6' list" $ do
      "d8:intervali0e\
       \5:peers0:\
       \6:peers60:\
      \e" `shouldBe`
        AnnounceInfo Nothing Nothing 0 Nothing (CompactPeerList []) Nothing

    it "fails on invalid combinations of the peer lists" $ do
      BE.decode "d8:intervali0e\
                 \5:peers0:\
                 \6:peers6le\
                \e"
        `shouldBe` (Left
        "PeerList: the `peers6' field value should contain \
                  \*compact* peer list" :: BE.Result AnnounceInfo)

      BE.decode "d8:intervali0e\
                 \5:peersle\
                 \6:peers60:\
                \e"
        `shouldBe` (Left
        "PeerList: non-compact peer list provided, \
                  \but the `peers6' field present" :: BE.Result AnnounceInfo)

    it "properly bencoded (iso)" $ property $ \ info ->
      BE.decode (BL.toStrict (BE.encode info))
        `shouldBe` Right (info :: AnnounceInfo)

  describe "Scrape" $ do
    return ()
