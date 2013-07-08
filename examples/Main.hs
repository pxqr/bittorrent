module Main (main) where

import Control.Monad
import Network.BitTorrent
import Network.BitTorrent.Extension
import System.Environment
import Control.Monad.Trans

main :: IO ()
main = do
  [path]  <- getArgs
  torrent <- fromFile path
  print (contentLayout "./" (tInfo torrent))
  client  <- newClient 100 [ExtDHT]
  swarm   <- newLeecher  client torrent
  storage <- swarm `bindTo`  "/tmp/"
  ppStorage storage >>= print
  discover swarm $ do
    liftIO $ print "connected to peer"
    forever $ do
      liftIO (getCurrentProgress client >>= print)
      exchange storage
    liftIO $ print "disconnected"
