{-# OPTIONS -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative
import Control.DeepSeq
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Serialize
import Network.BitTorrent as BT


instance NFData BlockIx where
  rnf (BlockIx a b c) = a `deepseq` b `deepseq` rnf c

instance NFData Block where
  rnf (Block a b c) = a `deepseq` b `deepseq` rnf c

instance NFData Bitfield where
  rnf = rnf . bfBits

instance NFData Message where
  rnf (Have i)     = rnf i
  rnf (Bitfield b) = rnf b
  rnf (Request  b) = rnf b
  rnf (Piece    b) = rnf b
  rnf (Cancel   b) = rnf b
  rnf (Port     i) = rnf i
  rnf _ = ()  -- other fields are forced by pattern matching

encodeMessages :: [Message] -> ByteString
encodeMessages xs = runPut (mapM_ put xs)

decodeMessages :: ByteString -> Either String [Message]
decodeMessages = runGet (many get)

bitfieldMin :: Int -> Maybe Int
bitfieldMin n = findMin (BT.empty n)

bitfieldMax :: Int -> Maybe Int
bitfieldMax n = findMax (BT.empty n)

bitfieldDiff :: Int -> Bitfield
bitfieldDiff n = BT.empty n `difference` BT.empty n

bitfieldInter :: Int -> Bitfield
bitfieldInter n = BT.empty n `intersection` BT.empty n

bitfieldUnion :: Int -> Bitfield
bitfieldUnion n = BT.empty n `union` BT.empty n

selectionStrictFirst :: Int -> Maybe Int
selectionStrictFirst n = strictFirst (BT.empty n) (BT.empty n) []

main :: IO ()
main = do
  let datas = replicate 10000 (Request (BlockIx 0 0 0))
  let m = 1024 * 1024

  defaultMain
    [ datas `deepseq` bench "message/encode"   $ nf encodeMessages datas
    , let binary = encodeMessages datas in
      binary `deepseq` bench "message/decode"  $ nf decodeMessages binary

      -- ~ 256KiB * 10M = 2.5TiB
    , bench "bitfield/min"          $ nf bitfieldMin   (10 * m)
    , bench "bitfield/max"          $ nf bitfieldMax   (10 * m)
    , bench "bitfield/difference"   $ nf bitfieldDiff  (10 * m)
    , bench "bitfield/intersection" $ nf bitfieldInter (10 * m)
    , bench "bitfield/union"        $ nf bitfieldUnion (10 * m)

    , bench "selection/strictFirst" $ nf selectionStrictFirst  (10 * m)
    ]