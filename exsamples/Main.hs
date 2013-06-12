{-# LANGUAGE ViewPatterns #-}
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

    forever $ do
      e <- awaitEvent
      case e of
        Available bf
          | Just m <- findMin bf -> yieldEvent (Want (BlockIx m 0 10))
          |     otherwise        -> return ()
        Want     bix -> liftIO $ print bix
        Fragment blk -> liftIO $ print (ppBlock blk)


  print "Bye-bye! =_="