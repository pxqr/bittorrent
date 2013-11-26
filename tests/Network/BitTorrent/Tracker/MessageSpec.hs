{-# OPTIONS -fno-warn-orphans #-}
module Network.BitTorrent.Tracker.MessageSpec (spec) where

import Control.Applicative
import Data.BEncode as BE
import Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Word
import Network
import Network.URI
import Test.Hspec
import Test.QuickCheck
--import Network.HTTP.Types.URI

import Data.Torrent.InfoHashSpec ()
import Data.Torrent.ProgressSpec ()
import Network.BitTorrent.Core.PeerIdSpec ()

import Network.BitTorrent.Tracker.Message


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

baseURI :: URI
baseURI = fromJust $ parseURI "http://a"

spec :: Spec
spec = do
  describe "Announce" $ do
    return ()
  describe "Scrape" $ do
    return ()
