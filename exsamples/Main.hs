module Main (main) where

import Data.Bitfield
import Network.BitTorrent
import System.Environment


main :: IO ()
main = do
  [path] <- getArgs
  torrent <- fromFile path

  client  <- newClient []
  swarm   <- newLeacher  client torrent

  discover swarm $ \se -> do
    peers <- getPeerList se
    print peers

  print "Bye-bye!"