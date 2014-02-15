{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
module Main (main) where
import Control.Concurrent
import Control.Monad.Trans
import Options.Applicative
import System.Environment
import System.Exit
import System.IO
import Text.Read

import Network.BitTorrent


{-----------------------------------------------------------------------
--  Command line arguments
-----------------------------------------------------------------------}

data TorrentBox = forall s. TorrentSource s => TorrentBox { unTorrentBox :: s }

data Args = Args
  { topic       :: TorrentBox
  , contentDir  :: FilePath
  }

argsParser :: Parser Args
argsParser = Args <$> (TorrentBox <$> infohashP <|> TorrentBox <$> torrentP)
                  <*> destDirP
  where
    infohashP :: Parser InfoHash
    infohashP = argument readMaybe
                (metavar "SHA1"     <> help "infohash of torrent file")

    torrentP :: Parser FilePath
    torrentP  = argument Just
                ( metavar "FILE"
               <> help    "A .torrent file"
                )

    destDirP :: Parser FilePath
    destDirP  = argument Just
                ( metavar "DIR"
               <> help    "Directory to put content"
                )

argsInfo :: ParserInfo Args
argsInfo = info (helper <*> argsParser)
   ( fullDesc
  <> progDesc "A simple CLI bittorrent client"
  <> header   "foo"
   )

{-----------------------------------------------------------------------
--  Client
-----------------------------------------------------------------------}

run :: Args -> BitTorrent ()
run (Args (TorrentBox t) dir)  = do
  h <- openHandle dir t
  start h
  liftIO $ threadDelay 10000000000

main :: IO ()
main = execParser argsInfo >>= simpleClient . run
