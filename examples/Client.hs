module Main (main) where

import Control.Concurrent
import Data.Default
import System.Environment
import Text.PrettyPrint.Class

import Network.BitTorrent


main :: IO ()
main = do
  [path]  <- getArgs
  torrent <- fromFile path
  let logger = \ _ _ _ _ -> return ()
  withClient def logger $ flip runBitTorrent $ do
    return ()
