{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Foldable
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as B
import Data.Torrent
import System.Environment
import System.Exit

checkInfo :: ByteString
checkInfo = "0221caf96aa3cb94f0f58d458e78b0fc344ad8bf"

ppHex :: B.ByteString -> B.ByteString
ppHex = L.toStrict . B.toLazyByteString . foldMap (B.primFixed B.word8HexFixed) . B.unpack

torrentFileName :: String
torrentFileName = "tests/dapper-dvd-amd64.iso.torrent"

main :: IO ()
main = do
  args <- getArgs
  let path = if length args == 0 then torrentFileName else head args

  Right t <- fromFile path

  BC.putStr "info hash: "
  BC.putStrLn (ppHex (tInfoHash t))

  let passed = checkInfo == ppHex (tInfoHash t)
  print passed
  if passed then exitSuccess else exitFailure
