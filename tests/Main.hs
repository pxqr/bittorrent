module Main (main) where

import Data.Word
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Encoding

import Network.BitTorrent as BT

prop_bitfieldDiff0 :: Bitfield -> Bool
prop_bitfieldDiff0 b = (b `difference` empty (8 * bitfieldByteCount b)) == b

prop_bitfieldDiff1 :: Bitfield -> Bool
prop_bitfieldDiff1 b = em `difference` b == em
  where
    em = empty (8 * bitfieldByteCount b)

prop_bitfieldMaxNothing :: Int -> Bool
prop_bitfieldMaxNothing n = findMax (empty (n `mod` 1024)) == Nothing

prop_bitfieldMinNothing :: Int -> Bool
prop_bitfieldMinNothing n = findMax (empty (n `mod` 1024)) == Nothing

prop_bitfieldMaxJust :: Word -> Bool
prop_bitfieldMaxJust n =
    let m = findMax (full (8 * s)) in
    if n == 0 then m == Nothing
    else m == Just ((s * 8) - 1)
  where
    s = fromIntegral n `mod` 1024

prop_bitfieldMinJust :: Word -> Bool
prop_bitfieldMinJust n =
  let m = findMin (full (fromIntegral n `mod` 1024)) in
  if n == 0 then m == Nothing
  else m == Just 0


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
       , testProperty "prop_bitfieldMinNothing" prop_bitfieldMinNothing
       , testProperty "prop_bitfieldMaxNothing" prop_bitfieldMaxNothing
       , testProperty "prop_bitfieldMaxJust"    prop_bitfieldMaxJust
       , testProperty "prop_bitfieldMinJust"    prop_bitfieldMinJust
       ]