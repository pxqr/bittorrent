{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Torrent

import System.Environment
import System.Exit

checkInfo :: String
checkInfo = "0221caf96aa3cb94f0f58d458e78b0fc344ad8bf"

torrentFileName :: String
torrentFileName = "res/dapper-dvd-amd64.iso.torrent"

main :: IO ()
main = do
  args <- getArgs
  let path = if length args == 0 then torrentFileName else head args

  t <- fromFile path

  BC.putStr "info hash: "
  print (ppInfoHash (tInfoHash t))

  let passed = checkInfo == show (ppInfoHash (tInfoHash t))

  print passed
  if passed then exitSuccess else exitFailure
