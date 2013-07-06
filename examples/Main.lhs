A very helpful and literate comment.
% A very helpful comment for very helpful and very literate comment.
\begin{code}
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

  client  <- newClient 2 []
  swarm   <- newLeecher  client torrent

  storage <- swarm `bindTo`  "/tmp/"

  ppStorage storage >>= print

  discover swarm $ do
    liftIO $ print "connected to peer"
    forever $ do
      liftIO (getCurrentProgress client >>= print)
      exchange storage
    liftIO $ print "disconnected"
\end{code}