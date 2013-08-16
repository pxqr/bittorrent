-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent
       ( module Data.Torrent.Metainfo

       , TorrentLoc(..), TorrentMap, Progress(..)
       , ThreadCount, SessionCount

       , ClientSession( clientPeerId, allowedExtensions )
       , withDefaultClient, defaultThreadCount, defaultPorts
       , addTorrent
       , removeTorrent

       , getCurrentProgress
       , getPeerCount
       , getSwarmCount
       , getSessionCount
       , getSwarm
       , getStorage
       , getTorrentInfo
       , getTorrentInfoStr

         -- * Torrent Groups
       , ClientLoc (..), ppClientLoc
       , concreteLoc, concretePath
       , addTorrents
       , removeTorrents

         -- * Extensions
       , Extension
       , defaultExtensions
       , ppExtension
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List as L
import Data.HashMap.Strict as HM
import Network
import Text.Read
import Text.PrettyPrint
import System.Directory
import System.FilePath

import Data.Torrent.Metainfo
import Network.BitTorrent.Sessions.Types
import Network.BitTorrent.Sessions
import Network.BitTorrent.Extension
import Network.BitTorrent.Tracker


-- TODO remove fork from Network.BitTorrent.Exchange
-- TODO make all forks in Internal.

-- | Client session with default parameters. Use it for testing only.
withDefaultClient :: PortNumber -> PortNumber -> (ClientSession -> IO ()) -> IO ()
withDefaultClient listPort dhtPort action = do
  withClientSession defaultThreadCount [] listPort dhtPort action

getTorrentInfoStr :: ClientSession -> String -> IO (Maybe Torrent)
getTorrentInfoStr cs str
  | Just infohash <- readMaybe str = getTorrentInfo cs infohash
  |            otherwise           = return Nothing

{-----------------------------------------------------------------------
    Torrent management
-----------------------------------------------------------------------}

-- | Register torrent and start downloading.
addTorrent :: ClientSession -> TorrentLoc -> IO ()
addTorrent cs loc @ TorrentLoc {..} = do
  registerTorrent  cs loc
  openSwarmSession cs loc
  return ()

-- | Unregister torrent and stop all running sessions.
removeTorrent :: ClientSession -> InfoHash ->  IO ()
removeTorrent = unregisterTorrent

{-
-- | The same as 'removeTorrrent' torrent, but delete all torrent
--   content files.
deleteTorrent :: ClientSession -> TorrentLoc -> IO ()
deleteTorrent ClientSession {..} TorrentLoc {..} = undefined
-}

{-----------------------------------------------------------------------
    Torrent group management
-----------------------------------------------------------------------}
-- TODO better name

data ClientLoc = ClientLoc
  { tdir :: FilePath -- ^ Path to directory with .torrent files.
  , ddir :: FilePath -- ^ Path to directory to place content.
  } deriving (Show, Eq)

ppClientLoc :: ClientLoc -> Doc
ppClientLoc ClientLoc {..} =
  text "torrent directory" <+> text tdir $$
  text "data directory"    <+> text ddir

concretePath :: ClientLoc -> FilePath -> FilePath
concretePath ClientLoc {..} relPath = tdir </> relPath

concreteLoc :: ClientLoc -> FilePath -> TorrentLoc
concreteLoc loc @ ClientLoc {..} relPath
  = TorrentLoc (concretePath loc relPath) ddir

addTorrents :: ClientSession -> ClientLoc -> IO ()
addTorrents ses loc @ ClientLoc {..} = do
    paths <- L.filter isTorrentPath <$> getDirectoryContents tdir
    forM_ paths $ handle handler . addTorrent ses . concreteLoc loc
  where
    handler :: SomeException -> IO ()
    handler = print

removeTorrents :: ClientSession -> IO ()
removeTorrents cs = do
  tm <- getRegistered cs
  forM_ (keys tm) (removeTorrent cs)

{-
deleteTorrents :: ClientSession -> IO ()
deleteTorrents = undefined
-}