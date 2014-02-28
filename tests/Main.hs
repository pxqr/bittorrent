{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Exception
import Control.Monad
import Data.Functor
import Data.Maybe
import System.Directory
import System.Exit
import System.Environment
import System.FilePath
import System.Process
import Text.Printf
import Test.Hspec

import Config
import Spec


type Command = String
type Descr = (ClientName, ClientOpts -> FilePath -> Command)

torrents :: [FilePath]
torrents =
    [ "dapper-dvd-amd64-iso.torrent"
    , "pkg.torrent"
    , "testfile.torrent"
    ]

rtorrentSessionDir :: String
rtorrentSessionDir = "rtorrent-sessiondir"

sessionName :: String -- screen session name
sessionName = "bittorrent-testsuite"

clients :: [Descr]
clients =
  [ ("rtorrent"
    , \ ClientOpts {..} tfile -> printf
     "rtorrent -p %i-%i -O dht=on -O dht_port=%i -O session=%s %s"
     (fromEnum peerPort) (fromEnum peerPort) (fromEnum nodePort)
      rtorrentSessionDir tfile
    )
  ]

setupEnv :: EnvOpts -> IO (Maybe ())
setupEnv EnvOpts {..}
  | Just client <- testClient
  , Just mkCmd  <- lookup client clients = do
    _ <- printf "Setting up %s\n" client

    let torrentPath = "testfile.torrent"
    let tmpDir = "res"
    let runner = printf "screen -dm -S %s %s" sessionName
                 (mkCmd remoteOpts torrentPath)

    wd <- getCurrentDirectory
    createDirectoryIfMissing True (wd </> tmpDir </> rtorrentSessionDir)
    _ <- createProcess (shell runner) { cwd = Just (wd </> tmpDir) }

    return (Just ())

  | Just client <- testClient = do
    _ <- printf "Bad client `%s`, use one of %s\n" client (show (fst <$> clients))
    return Nothing

  | otherwise = do
    _ <- printf "Running without remote client\n"
    return (Just ())

terminateEnv :: IO ()
terminateEnv = do
  _ <- printf "closing screen session: %s\n" sessionName
  _ <- system (printf "screen -S %s -X quit" sessionName)
  return ()

runTestSuite :: [String] -> IO ExitCode
runTestSuite args = do
  _ <- printf "running hspec test suite with args: %s\n" (show args)
  catch (withArgs args (hspec spec) >> return ExitSuccess) return

main :: IO ()
main = do
  (envOpts, suiteArgs) <- getOpts
  running <- setupEnv     envOpts
  code    <- runTestSuite suiteArgs
  when (isJust running) $ do
    terminateEnv
  exitWith code
