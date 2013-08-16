-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Do not add other (than this) test suites without need. Do not use
--   linux-specific paths, use 'filepath' and 'directory' machinery.
--
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main (main) where

import Control.Applicative hiding (empty)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import Data.List as L
import Data.Maybe
import Data.Word
import Data.Serialize as S
import Data.Text as T

import Network
import Network.URI

import Test.QuickCheck as QC
import Test.Framework as Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Aeson as JSON
import Data.BEncode as BE
import Data.Torrent.Block
import Data.Torrent.Bitfield as BF
import Data.Torrent.Metainfo
import Network.BitTorrent.Exchange.Protocol
import Network.BitTorrent.Tracker
import Network.BitTorrent.Tracker.Protocol
import Network.BitTorrent.Tracker.HTTP
import Network.BitTorrent.Peer

-- import Debug.Trace


data T a = T


prop_properBEncode :: Show a => BEncodable a => Eq a => T a -> a -> Bool
prop_properBEncode _ expected = actual == Right expected
  where
    actual = decoded $ Lazy.toStrict $ encoded expected

instance Arbitrary URI where
  arbitrary = pure $ fromJust
              $ parseURI "http://exsample.com:80/123365_asd"

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

{-----------------------------------------------------------------------
    Handshake
-----------------------------------------------------------------------}

instance Arbitrary PeerId where
  arbitrary = azureusStyle <$> pure defaultClientId
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

instance Arbitrary ScrapeInfo where
  arbitrary = ScrapeInfo <$> arbitrary <*> arbitrary
                         <*> arbitrary <*> arbitrary

-- | Note that in 6 esample we intensionally do not agree with
-- specification, because taking in account '/' in query parameter
-- seems to be meaningless.  (And thats because other clients do not
-- chunk uri by parts) Moreover in practice there should be no
-- difference. (I think so)
--
test_scrape_url :: [Framework.Test]
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

instance Arbitrary Lazy.ByteString where
  arbitrary = Lazy.pack <$> arbitrary

instance Arbitrary BlockIx where
  arbitrary = BlockIx <$> positive <*> positive <*> positive

instance Arbitrary Block where
  arbitrary = Block <$> positive <*> positive <*> arbitrary

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
    , pure $ Bitfield $ BF.haveNone 0
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

allTests :: [Framework.Test]
allTests =
  [ -- handshake module
    testProperty "handshake encoding" $
      prop_cerealEncoding  (T :: T Handshake)
  , testProperty "message encoding" prop_messageEncoding
  ] ++ test_scrape_url
    ++
  [ testProperty "scrape bencode" $
      prop_properBEncode (T :: T ScrapeInfo)
  ]

main :: IO ()
main = defaultMain allTests
