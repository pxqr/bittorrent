{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Exception
import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe
import System.Exit
import System.Environment
import System.Process
import System.Directory
import Text.Printf

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

clients :: [Descr]
clients =
  [ ("rtorrent"
    , \ ClientOpts {..} tfile -> printf
     "rtorrent -p %i-%i -O dht=on -O dht_port=%i -O session=rtorrent-sessiondir %s"
     (fromEnum peerPort) (fromEnum peerPort) (fromEnum nodePort) tfile
    )
  ]

sessionName :: String -- screen session name
sessionName = "bittorrent-testsuite"

setupEnv :: EnvOpts -> IO (Maybe ())
setupEnv EnvOpts {..}
  | Just client <- testClient
  , Just mkCmd  <- lookup client clients = do
    let runner = printf "screen -dm -S %s %s" sessionName
                 (mkCmd remoteOpts "testfile.torrent")
    dir <- getCurrentDirectory
    _   <- createProcess (shell runner) { cwd = Just (dir ++ "/res") }
    return (Just ())

  | Just client <- testClient = do
    printf "Bad client `%s`, use one of %s" client (show (fst <$> clients))
    return Nothing

  | isNothing testClient = do
    printf "Running without remote client"
    return (Just ())

terminateEnv :: IO ()
terminateEnv = do
  printf "closing screen session: %s" sessionName
  _ <- system (printf "screen -S %s -X quit" sessionName)
  return ()

runTestSuite :: [String] -> IO ExitCode
runTestSuite args = do
  printf "running hspec test suite with args: %s\n" (show args)
  catch (withArgs args hspecMain >> return ExitSuccess) return

main :: IO ()
main = do
  (envOpts, suiteArgs) <- getOpts
  running <- setupEnv     envOpts
  code    <- runTestSuite suiteArgs
  when (isJust running) $ do
    terminateEnv
  exitWith code
