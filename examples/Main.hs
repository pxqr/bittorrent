module Main (main) where

import Control.Concurrent
import Network.BitTorrent
import Network.BitTorrent.Sessions
import System.Environment

main :: IO ()
main = do
  [path]  <- getArgs
  torrent <- fromFile path
  print (contentLayout "./" (tInfo torrent))
  let loc = TorrentLoc path "/tmp"

  withDefaultClient (head defaultPorts) 3000 $ \ client -> do
    openSwarmSession client loc
    threadDelay 1000000000000
    return ()
