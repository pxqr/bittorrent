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

bitfieldDiff :: Int -> Bitfield
bitfieldDiff n = BT.empty n `difference` BT.empty n

bitfieldMin :: Int -> Maybe Int
bitfieldMin n = findMin (BT.empty n)

bitfieldMax :: Int -> Maybe Int
bitfieldMax n = findMax (BT.empty n)

main :: IO ()
main = do
  let datas = replicate 10000 (Request (BlockIx 0 0 0))

  defaultMain
    [ datas `deepseq` bench "message/encode"   $ nf encodeMessages datas
    , let binary = encodeMessages datas in
      binary `deepseq` bench "message/decode"  $ nf decodeMessages binary

    , bench "bitfield/difference"  $ nf bitfieldDiff 1000000
    , bench "bitfield/min"         $ nf bitfieldMin  10000000
    , bench "bitfield/max"         $ nf bitfieldMax  10000000
    ]