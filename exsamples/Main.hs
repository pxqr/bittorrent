{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Control.Concurrent
import Data.Bitfield
import Network.BitTorrent
import System.Environment
import Control.Monad.Reader
import Data.IORef

main :: IO ()
main = do
  [path] <- getArgs
  torrent <- fromFile path

  client  <- newClient []
  swarm   <- newLeacher  client torrent

  ref <- newIORef 0

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
        Fragment blk -> do

          liftIO $ do
            readIORef ref >>= print
            modifyIORef ref succ
            print (ppBlock blk)

          yieldEvent (Want (BlockIx 0 0 (16 * 1024)))


  print "Bye-bye! =_="