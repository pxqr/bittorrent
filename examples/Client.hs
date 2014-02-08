module Main (main) where
import System.Environment
import System.Exit
import System.IO
import Network.BitTorrent

parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    [path] -> return path
    _      -> do
      hPutStrLn stderr "Usage: client file.torrent"
      exitFailure

main :: IO ()
main = do
  path    <- parseArgs
  torrent <- fromFile path
  simpleClient $ do
    h <- openTorrent torrent
    return ()
