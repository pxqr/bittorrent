{-# OPTIONS -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative
import Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.Torrent
import Network.Torrent
import Network.URI

positive :: Gen Int
positive = fromIntegral <$> (arbitrary :: Gen Word32)

instance Arbitrary ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary BlockIx where
  arbitrary = BlockIx <$> positive <*> positive <*> positive

instance Arbitrary Block where
  arbitrary = Block <$> positive <*> positive <*> arbitrary

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
  arbitrary = (hash . B.pack) <$> vectorOf 20 arbitrary

instance Arbitrary Handshake where
  arbitrary = defaultHandshake <$> arbitrary <*> arbitrary

data T a = T

prop_encoding :: (Serialize a, Eq a) => T a -> [a] -> Bool
prop_encoding _ msgs = decode (encode msgs) == Right msgs

test_scrape_url :: Bool
test_scrape_url = check `all` tests
  where
    check (iu, ou) = (parseURI iu >>= (`scrapeURL` []) >>= return . show) == ou
    tests =
      [ ("http://example.com/announce"        , Just "http://example.com/scrape")
      , ("http://example.com/x/announce"      , Just "http://example.com/x/scrape")
      , ("http://example.com/announce.php"    , Just "http://example.com/scrape.php")
      , ("http://example.com/a"               , Nothing)
      , ("http://example.com/announce?x2%0644", Just "http://example.com/scrape?x2%0644")
      , ("http://example.com/announce?x=2/4"  , Nothing)
      , ("http://example.com/x%064announce"   , Nothing)
      ]

main :: IO ()
main = do
  defaultMain
       [ testProperty "Message encode <-> decode" $
            prop_encoding (T :: T Message)

       , testProperty "PeerID encode <-> decode" $
            prop_encoding (T :: T PeerID)

       , testProperty "Handshake encode <-> decode" $
            prop_encoding (T :: T Handshake)

       , testProperty "Scrape URL" $
            test_scrape_url
       ]