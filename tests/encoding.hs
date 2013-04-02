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

import Network.Torrent.PWP

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


encodeMessages :: [Message] -> ByteString
encodeMessages xs = runPut (mapM_ put xs)

decodeMessages :: ByteString -> Either String [Message]
decodeMessages = runGet (many get)

-- | TODO move tests
prop_encoding :: [Message] -> Bool
prop_encoding msgs = decodeMessages (encodeMessages msgs) == Right msgs


main :: IO ()
main = do
  defaultMain
       [ testProperty "encode <-> decode" prop_encoding
       ]