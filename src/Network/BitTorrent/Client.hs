{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Network.BitTorrent.Client
       ( -- * Options
         Options (..)

         -- * Client session
       , Client

         -- ** Session data
       , clientPeerId
       , clientListenerPort
       , allowedExtensions

         -- ** Session initialization
       , newClient
       , closeClient
       , withClient
       , simpleClient

         -- * BitTorrent monad
       , MonadBitTorrent (..)
       , BitTorrent
       , runBitTorrent
       , getClient

         -- * Handle
       , TorrentSource (..)
       , closeHandle

       , start
       , pause
       , stop
       ) where

import Control.Exception
import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.Trans

import Data.Default
import Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text
import Network

import           Data.Torrent
import           Data.Torrent.InfoHash
import           Data.Torrent.Magnet
import           Network.BitTorrent.Client.Types
import           Network.BitTorrent.Client.Handle
import           Network.BitTorrent.Core
import           Network.BitTorrent.DHT      as DHT      hiding (Options)
import           Network.BitTorrent.Tracker  as Tracker  hiding (Options)
import           Network.BitTorrent.Exchange as Exchange hiding (Options)
import qualified Network.BitTorrent.Exchange as Exchange (Options(..))


data Options = Options
  { optFingerprint :: Fingerprint
  , optName        :: Text
  , optPort        :: PortNumber
  , optExtensions  :: [Extension]
  , optNodeAddr    :: NodeAddr IPv4
  , optBootNode    :: Maybe (NodeAddr IPv4)
  }

instance Default Options where
  def = Options
    { optFingerprint = def
    , optName        = "hs-bittorrent"
    , optPort        = 6882
    , optExtensions  = []
    , optNodeAddr    = "0.0.0.0:6882"
    , optBootNode    = Nothing
    }

exchangeOptions :: PeerId -> Options -> Exchange.Options
exchangeOptions pid Options {..} = Exchange.Options
  { optPeerAddr = PeerAddr (Just pid) (peerHost def) optPort
  , optBacklog  = optBacklog def
  }

connHandler :: MVar (HashMap InfoHash Handle) -> Exchange.Handler
connHandler _tmap = undefined

newClient :: Options -> LogFun -> IO Client
newClient opts @ Options {..} logger = do
  pid  <- genPeerId
  tmap <- newMVar HM.empty
  tmgr <- Tracker.newManager def (PeerInfo pid Nothing optPort)
  emgr <- Exchange.newManager (exchangeOptions pid opts) (connHandler tmap)
  node <- do
    node <- startNode defaultHandlers def optNodeAddr logger
    runDHT node $ bootstrap (maybeToList optBootNode)
    return node
  return Client
    { clientPeerId       = pid
    , clientListenerPort = optPort
    , allowedExtensions  = toCaps optExtensions
    , trackerManager     = tmgr
    , exchangeManager    = emgr
    , clientNode         = node
    , clientTorrents     = tmap
    , clientLogger       = logger
    }

closeClient :: Client -> IO ()
closeClient Client {..} = do
  Exchange.closeManager exchangeManager
  Tracker.closeManager  trackerManager
  DHT.stopNode clientNode
  return ()
--  closeNode clientNode

withClient :: Options -> LogFun -> (Client -> IO a) -> IO a
withClient opts lf action = bracket (newClient opts lf) closeClient action

-- | Run bittorrent client with default options and log to @stderr@.
--
--   For testing purposes only.
--
simpleClient :: BitTorrent () -> IO ()
simpleClient m = do
  runStderrLoggingT $ LoggingT $ \ logger -> do
    withClient def logger (`runBitTorrent` m)

{-----------------------------------------------------------------------
--  Torrent identifiers
-----------------------------------------------------------------------}

class TorrentSource s where
  openHandle :: FilePath -> s -> BitTorrent Handle

instance TorrentSource InfoHash where
  openHandle path ih = openMagnet path (nullMagnet ih)
  {-# INLINE openHandle #-}

instance TorrentSource Magnet where
  openHandle = openMagnet
  {-# INLINE openHandle #-}

instance TorrentSource InfoDict where
  openHandle path dict = openTorrent path (nullTorrent dict)
  {-# INLINE openHandle #-}

instance TorrentSource Torrent where
  openHandle = openTorrent
  {-# INLINE openHandle #-}

instance TorrentSource FilePath where
  openHandle contentDir torrentPath = do
    t <- liftIO $ fromFile torrentPath
    openTorrent contentDir t
  {-# INLINE openHandle #-}