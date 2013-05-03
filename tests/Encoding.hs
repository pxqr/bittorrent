{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fno-warn-orphans #-}
module Encoding where

import Control.Applicative
import Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Network.URI

import Data.Torrent
import Network.BitTorrent


positive :: Gen Int
positive = fromIntegral <$> (arbitrary :: Gen Word32)

instance Arbitrary ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary BlockIx where
  arbitrary = BlockIx <$> positive <*> positive <*> positive

instance Arbitrary Block where
  arbitrary = Block <$> positive <*> positive <*> arbitrary

deriving instance Arbitrary Bitfield

instance Arbitrary Message where
  arbitrary = oneof
    [ pure KeepAlive
    , pure Choke
    , pure Unchoke
    , pure Interested
    , pure NotInterested
    , Have <$> positive
    , Bitfield <$> arbitrary
    , Request <$> arbitrary
    , Piece <$> arbitrary
    , Cancel <$> arbitrary
    , Port <$> choose (0, fromIntegral (maxBound :: Word16))
    ]

instance Arbitrary PeerID where
  arbitrary = azureusStyle <$> pure defaultClientID
                           <*> arbitrary
                           <*> arbitrary

instance Arbitrary InfoHash where
  arbitrary = (hash . B.pack) <$> arbitrary

instance Arbitrary Handshake where
  arbitrary = defaultHandshake <$> arbitrary <*> arbitrary

data T a = T

prop_encoding :: (Serialize a, Eq a) => T a -> [a] -> Bool
prop_encoding _ msgs = decode (encode msgs) == Right msgs

-- | Note that in 6 esample we intensionally do not agree with specification,
--   because taking in account '/' in query parameter seems to be meaningless.
--   (And thats because other clients do not chunk uri by parts)
--   Moreover in practice there should be no difference. (I hope)
--
test_scrape_url :: [Test]
test_scrape_url = zipWith mkTest [1 :: Int ..] (check `map` tests)
  where
    check (iu, ou) = (parseURI iu >>= (`scrapeURL` []) >>= return . show) == ou
    tests =
      [ ("http://example.com/announce"        , Just "http://example.com/scrape")
      , ("http://example.com/x/announce"      , Just "http://example.com/x/scrape")
      , ("http://example.com/announce.php"    , Just "http://example.com/scrape.php")
      , ("http://example.com/a"               , Nothing)
      , ("http://example.com/announce?x2%0644", Just "http://example.com/scrape?x2%0644")
      , ("http://example.com/announce?x=2/4"  , Just "http://example.com/scrape?x=2/4")
--      , ("http://example.com/announce?x=2/4"  , Nothing) -- by specs
      , ("http://example.com/x%064announce"   , Nothing)
      ]

    mkTest i = testProperty ("scrape test #" ++ show i)
