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

import Network.Torrent

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

instance Arbitrary Handshake where
  arbitrary = defaultHandshake
                <$> (B.pack <$> (vectorOf 20 arbitrary))
                <*> arbitrary

data T a = T

prop_encoding :: (Serialize a, Eq a) => T a -> [a] -> Bool
prop_encoding _ msgs = decode (encode msgs) == Right msgs


main :: IO ()
main = do
  defaultMain
       [ testProperty "Message encode <-> decode" $
            prop_encoding (T :: T Message)

       , testProperty "PeerID encode <-> decode" $
            prop_encoding (T :: T PeerID)

       , testProperty "Handshake encode <-> decode" $
            prop_encoding (T :: T Handshake)
       ]