module Main (main) where

import Control.Concurrent
import Data.Bitfield
import Network.BitTorrent
import System.Environment
import Control.Monad.Reader


main :: IO ()
main = do
  [path] <- getArgs
  torrent <- fromFile path

  client  <- newClient []
  swarm   <- newLeacher  client torrent

  discover swarm $ do
    addr <- asks connectedPeerAddr
    liftIO $ print $ "connected to" ++ show addr
    e <- awaitEvent
    liftIO $ print e
    liftIO $ threadDelay (100 * 1000000)

  print "Bye-bye! =_="