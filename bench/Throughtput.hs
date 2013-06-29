{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE PatternGuards #-}
module Main (main) where

import Control.Concurrent
import Data.Bitfield
import Network.BitTorrent
import System.Environment
import Control.Monad.Reader
import Data.IORef


main :: IO ()
main = do
  [path]  <- getArgs
  torrent <- fromFile path

  print (contentLayout "./" (tInfo torrent))

  client  <- newClient 100 []
  swarm   <- newLeecher  client torrent

  ref <- liftIO $ newIORef 0
  discover swarm $ do
    forever $ do
      e <- awaitEvent
      case e of
        Available bf
          | Just m <- findMin bf -> yieldEvent (Want (BlockIx m 0 10))
          |     otherwise        -> return ()
        Want     bix -> liftIO $ print bix
        Fragment blk -> do

          sc <- liftIO $ getSessionCount swarm
          addr <- asks connectedPeerAddr

          liftIO $ do
            x <- atomicModifyIORef ref (\x -> (succ x, x))
            if x `mod` 100 == 0
              then print (x, sc, addr)
              else return ()

          yieldEvent (Want (BlockIx 0 0 (16 * 1024)))


  print "Bye-bye! =_="