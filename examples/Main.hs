module Main (main) where

import Network.BitTorrent
import System.Environment

main :: IO ()
main = do
  [path]  <- getArgs
  torrent <- fromFile path
  print (contentLayout "./" (tInfo torrent))

  withDefaultClient 3000 3001 $ \ client ->
    addTorrent client $ TorrentLoc path "/tmp"
