{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Word
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Encoding

import Data.Bitfield as BT
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

prop_bitfieldMinCases :: Bool
prop_bitfieldMinCases = all mkTestCase
    [ ("\x0\x3", Just 8)
    , ("\x0\x127", Just 8)
    ]
  where
    mkTestCase (bs, res) = findMin (MkBitfield bs) == res

prop_bitfieldMaxCases :: Bool
prop_bitfieldMaxCases = all mkTestCase
    [ ("\x0\x3", Just 9)
    , ("\x0\x127", Just 13)
    ]
  where
    mkTestCase (bs, res) = findMax (MkBitfield bs) == res

prop_bitfieldMinJust :: Word -> Bool
prop_bitfieldMinJust n =
  let m = findMin (full (fromIntegral n `mod` 1024)) in
  if n == 0 then m == Nothing
  else m == Just 0

prop_bitfieldUnionIdentity :: Bitfield -> Bool
prop_bitfieldUnionIdentity b =
      ((b `union` empty (8 * bitfieldByteCount b)) == b)
  &&  ((empty (8 * bitfieldByteCount b) `union` b) == b)

prop_bitfieldUnionCommutative :: Bitfield -> Bitfield -> Bool
prop_bitfieldUnionCommutative a b = union a b == union b a

prop_bitfieldUnionAssociative :: Bitfield -> Bitfield -> Bitfield -> Bool
prop_bitfieldUnionAssociative a b c = union a (union b c) == union (union a b) c

prop_bitfieldUnionIdempotent :: Bitfield -> Bitfield -> Bool
prop_bitfieldUnionIdempotent a b = union a b == union a (union a b)

prop_bitfieldIntersectionIdentity :: Bitfield -> Bool
prop_bitfieldIntersectionIdentity b =
      ((b `intersection` full (8 * bitfieldByteCount b)) == b)
  &&  ((full (8 * bitfieldByteCount b) `intersection` b) == b)

prop_bitfieldIntersectionCommutative :: Bitfield -> Bitfield -> Bool
prop_bitfieldIntersectionCommutative a b = intersection a b == intersection b a

prop_bitfieldIntersectionAssociative :: Bitfield -> Bitfield -> Bitfield -> Bool
prop_bitfieldIntersectionAssociative a b c =
  intersection a (intersection b c) == intersection (intersection a b) c

prop_bitfieldIntersectionIndempotent :: Bitfield -> Bitfield -> Bool
prop_bitfieldIntersectionIndempotent a b = f b == f (f b)
  where
    f = intersection a

prop_bitfieldHaveCount :: Bitfield -> Bool
prop_bitfieldHaveCount b = listHaveCount (toList b) == haveCount b
  where
    listHaveCount = foldr f 0

    f :: Bool -> Int -> Int
    f byte count = fromEnum byte + count

prop_bitfieldCompeteness :: Bitfield -> Bool
prop_bitfieldCompeteness b = let (have, total) = completeness b in have <= total

main :: IO ()
main = defaultMain $
       [ testProperty "Message encode <-> decode"   $ prop_encoding (T :: T Message)
       , testProperty "PeerID encode <-> decode"    $ prop_encoding (T :: T PeerID)
       , testProperty "Handshake encode <-> decode" $ prop_encoding (T :: T Handshake)
       ]
       ++ test_scrape_url ++
       [
         testProperty "bitfield `difference` empty bitfield" prop_bitfieldDiff0
       , testProperty "empty bitfield `difference` bitfield" prop_bitfieldDiff1

       , testProperty "prop_bitfieldMinNothing"              prop_bitfieldMinNothing
       , testProperty "prop_bitfieldMaxNothing"              prop_bitfieldMaxNothing
       , testProperty "prop_bitfieldMaxJust"                 prop_bitfieldMaxJust
       , testProperty "prop_bitfieldMinJust"                 prop_bitfieldMinJust
       , testProperty "prop_bitfieldMinCases"                prop_bitfieldMinCases
       , testProperty "prop_bitfieldMaxCases"                prop_bitfieldMaxCases

       , testProperty "prop_bitfieldUnionIdentity"           prop_bitfieldUnionIdentity
       , testProperty "prop_bitfieldUnionCommutative"        prop_bitfieldUnionCommutative
       , testProperty "prop_bitfieldUnionAssociative"        prop_bitfieldUnionAssociative
       , testProperty "prop_bitfieldUnionIdempotent"         prop_bitfieldUnionIdempotent

       , testProperty "prop_bitfieldIntersectionIdentity"    prop_bitfieldIntersectionIdentity
       , testProperty "prop_bitfieldIntersectionCommutative" prop_bitfieldIntersectionCommutative
       , testProperty "prop_bitfieldIntersectionAssociative" prop_bitfieldIntersectionAssociative
       , testProperty "prop_bitfieldIntersectionIndempotent" prop_bitfieldIntersectionIndempotent

       , testProperty "prop_bitfieldHaveCount"               prop_bitfieldHaveCount
       , testProperty "prop_bitfieldCompeteness"             prop_bitfieldCompeteness
       ]