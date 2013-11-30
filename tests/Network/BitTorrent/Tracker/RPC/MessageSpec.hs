{-# LANGUAGE RecordWildCards  #-}
{-# OPTIONS -fno-warn-orphans #-}
module Network.BitTorrent.Tracker.RPC.MessageSpec
       ( spec
       , validateInfo
       , arbitrarySample
       ) where

import Control.Applicative
import Data.List as L
import Data.Maybe
import Data.Word
import Network
import Test.Hspec
import Test.QuickCheck

import Data.Torrent.InfoHashSpec ()
import Data.Torrent.ProgressSpec ()
import Network.BitTorrent.Core.PeerIdSpec ()

import Network.BitTorrent.Tracker.RPC.Message as Message
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
    peerList `shouldSatisfy` L.all (isNothing . peerID)
    fromJust respComplete + fromJust respIncomplete `shouldBe` L.length peerList
  where
    peerList = getPeerList respPeers

arbitrarySample :: Arbitrary a => IO a
arbitrarySample = L.head <$> sample' arbitrary

spec :: Spec
spec = do
  describe "Announce" $ do
    it "properly url encoded" $ property $ \ q ->
      parseAnnounceQuery (renderAnnounceQuery q)
        `shouldBe` Right q

  describe "Scrape" $ do
    return ()
