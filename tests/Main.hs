{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main (main) where

import Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import Data.List as L
import Data.Ord
import Data.Maybe
import Data.Word
import Data.Serialize as S
import Data.Text as T

import Network
import Network.URI

import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.BEncode as BE
import Data.Bitfield as BF
import Data.Torrent
import Network.BitTorrent as BT
import Network.BitTorrent.Exchange.Protocol
import Network.BitTorrent.Tracker
import Network.BitTorrent.Peer

-- import Debug.Trace


data T a = T

instance Arbitrary URI where
  arbitrary = pure $ fromJust
              $ parseURI "http://exsample.com:80/123365_asd"

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

{-----------------------------------------------------------------------
    Bitfield
-----------------------------------------------------------------------}
-- other properties are tested in IntervalSet

prop_completenessRange :: Bitfield -> Bool
prop_completenessRange bf = 0 <= c && c <= 1
  where
    c = completeness bf

prop_minMax :: Bitfield -> Bool
prop_minMax bf
  | BF.null bf = True
  | otherwise  = BF.findMin bf <= BF.findMax bf

prop_rarestInRange :: [Bitfield] -> Bool
prop_rarestInRange xs = case rarest xs of
  Just r  -> 0 <= r
          && r < totalCount (maximumBy (comparing totalCount) xs)
  Nothing -> True

{- this one should give pretty good coverage -}
prop_differenceDeMorgan :: Bitfield -> Bitfield -> Bitfield -> Bool
prop_differenceDeMorgan a b c =
  (a `BF.difference` (b `BF.intersection` c))
     == ((a `BF.difference` b) `BF.union` (a `BF.difference` c))
  &&
  (a `BF.difference` (b `BF.union` c))
     == ((a `BF.difference` b) `BF.intersection` (a `BF.difference` c))


{-----------------------------------------------------------------------
    Torrent
-----------------------------------------------------------------------}

prop_properBEncode :: Show a => BEncodable a => Eq a => T a -> a -> Bool
prop_properBEncode _ expected = actual == Right expected
  where
    actual = decoded $ Lazy.toStrict $ encoded expected


-- TODO tests for torrent: encoding <-> decoding
instance Arbitrary FileInfo where
  arbitrary = FileInfo <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ContentInfo where
  arbitrary = oneof
    [ SingleFile <$> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> arbitrary <*> arbitrary
    , MultiFile  <$> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Torrent where
  arbitrary = torrent <$> arbitrary
                 <*> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> pure Nothing <*> arbitrary

{-----------------------------------------------------------------------
    Handshake
-----------------------------------------------------------------------}

instance Arbitrary PeerID where
  arbitrary = azureusStyle <$> pure defaultClientID
                           <*> arbitrary
                           <*> arbitrary

instance Arbitrary InfoHash where
  arbitrary = (hash . B.pack) <$> arbitrary

instance Arbitrary Handshake where
  arbitrary = defaultHandshake <$> arbitrary <*> arbitrary

prop_cerealEncoding :: (Serialize a, Eq a) => T a -> [a] -> Bool
prop_cerealEncoding _ msgs = S.decode (S.encode msgs) == Right msgs

{-----------------------------------------------------------------------
    Tracker/Scrape
-----------------------------------------------------------------------}

-- | Note that in 6 esample we intensionally do not agree with
-- specification, because taking in account '/' in query parameter
-- seems to be meaningless.  (And thats because other clients do not
-- chunk uri by parts) Moreover in practice there should be no
-- difference. (I think so)
--
test_scrape_url :: [Test]
test_scrape_url = L.zipWith mkTest [1 :: Int ..] (check `L.map` tests)
  where
    check (iu, ou) = (parseURI iu >>= (`scrapeURL` [])
                      >>= return . show) == ou
    tests =
      [ (      "http://example.com/announce"
        , Just "http://example.com/scrape")
      , (      "http://example.com/x/announce"
        , Just "http://example.com/x/scrape")
      , (      "http://example.com/announce.php"
        , Just "http://example.com/scrape.php")
      , (      "http://example.com/a"    , Nothing)
      , (      "http://example.com/announce?x2%0644"
        , Just "http://example.com/scrape?x2%0644")
      , (      "http://example.com/announce?x=2/4"
        , Just "http://example.com/scrape?x=2/4")
--      , ("http://example.com/announce?x=2/4"  , Nothing) -- by specs
      , ("http://example.com/x%064announce"   , Nothing)
      ]

    mkTest i = testProperty ("scrape test #" ++ show i)

{-----------------------------------------------------------------------
    P2P/message
-----------------------------------------------------------------------}

positive :: Gen Int
positive = fromIntegral <$> (arbitrary :: Gen Word32)

instance Arbitrary ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary BlockIx where
  arbitrary = BlockIx <$> positive <*> positive <*> positive

instance Arbitrary Block where
  arbitrary = Block <$> positive <*> positive <*> arbitrary

instance Arbitrary Bitfield where
  arbitrary = mkBitfield <$> (succ . min 1000 <$> positive)
                         <*> arbitrary

instance Arbitrary PortNumber where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Word16)

instance Arbitrary Message where
  arbitrary = oneof
    [ pure KeepAlive
    , pure Choke
    , pure Unchoke
    , pure Interested
    , pure NotInterested
    , Have     <$> positive
    , Bitfield <$> arbitrary
    , Request  <$> arbitrary
    , Piece    <$> arbitrary
    , Cancel   <$> arbitrary
    , Port     <$> arbitrary
    ]
-- todo add all messages

prop_messageEncoding :: Message -> Bool
prop_messageEncoding msg @ (Bitfield bf)
  = case S.decode (S.encode msg) of
      Right (Bitfield bf') -> bf == adjustSize (totalCount bf) bf'
      _   -> False
prop_messageEncoding msg
  = S.decode (S.encode msg) == Right msg

{-----------------------------------------------------------------------
    Main
-----------------------------------------------------------------------}

main :: IO ()
main = defaultMain $
  [ -- bitfield module
    testProperty "completeness range"      prop_completenessRange
  , testProperty "rarest in range"         prop_rarestInRange
  , testProperty "min less that max"       prop_minMax
  , testProperty "difference de morgan"    prop_differenceDeMorgan

    -- torrent module
  , testProperty "file info encoding"      $
      prop_properBEncode (T :: T FileInfo)
  , testProperty "content info encoding"   $
      prop_properBEncode (T :: T ContentInfo)
  , testProperty "torrent encoding" $
      prop_properBEncode (T :: T Torrent)

    -- handshake module
  , testProperty "handshake encoding" $
      prop_cerealEncoding  (T :: T Handshake)
  , testProperty "message encoding" prop_messageEncoding

  ] ++ test_scrape_url
