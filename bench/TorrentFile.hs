{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.BEncode
import Data.ByteString as BS
import Data.Torrent
import Criterion.Main


tinyPath :: FilePath
tinyPath = "res/dapper-dvd-amd64.iso.torrent"

largePath :: FilePath
largePath = "res/pkg.torrent"

decoder :: ByteString -> Torrent
decoder bs = let Right r = decode bs in r

main :: IO ()
main = do
  !tinyBin  <- BS.readFile tinyPath
  !largeBin <- BS.readFile largePath

  defaultMain
    [ bench "read/tiny"  $ nf decoder tinyBin
    , bench "read/large" $ nf decoder largeBin
    ]