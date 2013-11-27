{-# OPTIONS -fno-warn-orphans #-}
module Network.BitTorrent.Tracker.MessageSpec (spec) where

import Control.Applicative
import Data.BEncode as BE
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy as BL
import Data.List as L
import Data.Maybe
import Data.Word
import Data.Text
import Network
import Network.URI
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Network.HTTP.Types.URI
import System.Random

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

parseUriQuery :: URI -> [(Text, Text)]
parseUriQuery = filterMaybes . parseQueryText . BC.pack . uriQuery
  where
    filterMaybes :: [(a, Maybe b)] -> [(a, b)]
    filterMaybes = catMaybes . L.map f
      where
        f (a, Nothing) = Nothing
        f (a, Just b ) = Just (a, b)

test = do
  let q = unGen arbitrary (mkStdGen 0) 0
  print $ renderAnnounceQuery baseURI q
  print $ parseUriQuery $ renderAnnounceQuery baseURI q

spec :: Spec
spec = do
  describe "Announce" $ do
    before test $
      it "properly url encoded" $ property $ \ q ->
      parseAnnounceQuery (parseUriQuery (renderAnnounceQuery baseURI q))
        `shouldBe` Right q

  describe "Scrape" $ do
    return ()
