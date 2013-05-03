module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Encoding

import Network.BitTorrent

prop_bitfieldDiff0 :: Bitfield -> Bool
prop_bitfieldDiff0 b = (b `difference` empty (8 * bitfieldByteCount b)) == b

prop_bitfieldDiff1 :: Bitfield -> Bool
prop_bitfieldDiff1 b = em `difference` b == em
  where
    em = empty (8 * bitfieldByteCount b)


main :: IO ()
main = defaultMain $
       [ testProperty "Message encode <-> decode" $
            prop_encoding (T :: T Message)

       , testProperty "PeerID encode <-> decode" $
            prop_encoding (T :: T PeerID)

       , testProperty "Handshake encode <-> decode" $
            prop_encoding (T :: T Handshake)
       ]
       ++ test_scrape_url ++
       [
         testProperty "bitfield `difference` empty bitfield" prop_bitfieldDiff0
       , testProperty "empty bitfield `difference` bitfield" prop_bitfieldDiff1
       ]