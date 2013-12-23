module Main where

import Spec
import System.Exit
import System.Environment
import System.Process
import Control.Exception
import Data.List
import Data.Maybe

clients :: [(String, String)]
clients = [
 ("rtorrent","rtorrent -p 51234-51234 res/testfile.torrent") ]

main :: IO ()
main = do
  args <- getArgs
  let cmd' = do
        cl <- listToMaybe $ reverse
                   $ map (tail . dropWhile (/='='))
                   $ filter (isPrefixOf "--bittorrent-client=") args
        cmd <- (++) "screen -dm -S bittorrent-testsuite " <$> lookup cl clients
        return cmd
  case cmd' of
    Just cmd -> do _ <- system "screen -S bittorrent-testsuite -X quit"
                   createProcess (shell cmd) >> return ()
    Nothing -> return ()

  let args' = (filter (not . isPrefixOf "--bittorrent-client=") args)
  code <- catch (withArgs args' hspecMain >> return ExitSuccess) return

  _ <- system "screen -S bittorrent-testsuite -X quit"
  exitWith code >> return ()
