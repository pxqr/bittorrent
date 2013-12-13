module Main (main) where

import Control.Concurrent
import Data.Default
import Data.Torrent
import Network.BitTorrent.Client
import System.Environment
import Text.PrettyPrint.Class


main :: IO ()
main = do
  [path]  <- getArgs
  torrent <- fromFile path
  client  <- newClient def
  addTorrent torrent client
  return ()