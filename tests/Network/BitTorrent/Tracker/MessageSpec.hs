{-# LANGUAGE RecordWildCards  #-}
{-# OPTIONS -fno-warn-orphans #-}
module Network.BitTorrent.Tracker.MessageSpec
       ( spec
       , validateInfo
       , arbitrarySample
       ) where

import Control.Applicative
import Control.Exception
import Data.BEncode as BE
import Data.List as L
import Data.Maybe
import Data.Word
import Network
import Test.Hspec
import Test.QuickCheck

import Data.Torrent.InfoHashSpec ()
import Data.Torrent.ProgressSpec ()
import Network.BitTorrent.Core.PeerIdSpec ()

import Network.BitTorrent.Tracker.Message as Message
import Network.BitTorrent.Core.PeerAddr


--prop_bencode :: Eq a => BEncode a => a -> Bool
--prop_bencode a = BE.decode (BL.toStrict (BE.encode a)) == return a

--prop_urlencode :: Eq a => URLDecoded a => URLEncoded a => a -> Bool
--prop_urlencode a = urlDecode (T.pack (urlEncode a)) == a

instance Arbitrary Event where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary PortNumber where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word16)

instance Arbitrary AnnounceQuery where
  arbitrary = AnnounceQuery
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary

validateInfo :: AnnounceQuery -> AnnounceInfo -> Expectation
validateInfo _ Message.Failure {..} = error "validateInfo: failure"
validateInfo AnnounceQuery {..}  AnnounceInfo {..} = do
    respComplete    `shouldSatisfy` isJust
    respIncomplete  `shouldSatisfy` isJust
    respMinInterval `shouldSatisfy` isNothing
    respWarning     `shouldSatisfy` isNothing
    peerList `shouldSatisfy` L.all (isNothing . peerId)
    fromJust respComplete + fromJust respIncomplete `shouldBe` L.length peerList
  where
    peerList = getPeerList respPeers

arbitrarySample :: Arbitrary a => IO a
arbitrarySample = L.head <$> sample' arbitrary

spec :: Spec
spec = do
  describe "AnnounceQuery" $ do
    it "properly url encoded" $ property $ \ q ->
      parseAnnounceQuery (renderAnnounceQuery q)
        `shouldBe` Right q

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

    it "parses peer list" $ do -- TODO
      "d8:intervali0e5:peerslee" `shouldBe`
        AnnounceInfo Nothing Nothing 0 Nothing (PeerList []) Nothing

  describe "Scrape" $ do
    return ()
