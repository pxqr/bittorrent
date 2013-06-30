module Main (main) where

import Control.Monad
import Network.BitTorrent
import System.Environment
import Control.Monad.Trans


main :: IO ()
main = do
  [path]  <- getArgs
  torrent <- fromFile path

  print (contentLayout "./" (tInfo torrent))

  client  <- newClient 100 []
  swarm   <- newLeecher  client torrent

  storage <- swarm `bindTo`  "/tmp/"

  ppStorage storage >>= print

  discover swarm $ do
    liftIO $ print "connected to peer"
    forever $ exchange storage
    liftIO $ print "disconnect to peer"