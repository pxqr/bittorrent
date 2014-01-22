{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

         -- * BitTorrent monad
       , BitTorrent
       , runBitTorrent
       , MonadBitTorrent (..)
       , getClient

         -- * Operations
       , addTorrent
       ) where

import Control.Concurrent.STM
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Default
import Data.Function
import Data.HashMap.Strict as HM
import Data.Ord
import Data.Text
import Network
import System.Log.FastLogger

import Data.Torrent
import Data.Torrent.InfoHash
import Network.BitTorrent.Client.Swarm
import Network.BitTorrent.Core
import Network.BitTorrent.DHT
import Network.BitTorrent.Exchange.Message


data Options = Options
  { fingerprint :: Fingerprint
  , name        :: Text
  , port        :: PortNumber
  , extensions  :: [Extension]
  , nodeAddr    :: NodeAddr IPv4
  , bootNode    :: NodeAddr IPv4
  }

instance Default Options where
  def = Options
    { fingerprint = def
    , name        = "hs-bittorrent"
    , port        = 6882
    , extensions  = []
    , nodeAddr    = "0.0.0.0:6882"
    }

data Client = Client
  { clientPeerId       :: !PeerId
  , clientListenerPort :: !PortNumber
  , allowedExtensions  :: !Caps
  , clientNode         :: !(Node IPv4)
  , clientTorrents     :: !(TVar (HashMap InfoHash Swarm))
  , clientLogger       :: !LogFun
-- , trackerClient     :: !(Manager)
  }

instance Eq Client where
  (==) = (==) `on` clientPeerId

instance Ord Client where
  compare = comparing clientPeerId

newClient :: Options -> LogFun -> IO Client
newClient Options {..} logger = do
  pid  <- genPeerId
  ts   <- newTVarIO HM.empty
  node <- runResourceT $ do
    node <- startNode handlers def nodeAddr logger
    runDHT node $ bootstrap [bootNode]
    return node

  return Client
    { clientPeerId       = pid
    , clientListenerPort = port
    , allowedExtensions  = toCaps extensions
    , clientTorrents     = ts
    , clientNode         = node
    , clientLogger       = logger
    }

closeClient :: Client -> IO ()
closeClient Client {..} = do
  return ()
--  closeNode clientNode

{-----------------------------------------------------------------------
--  BitTorrent monad
-----------------------------------------------------------------------}

class MonadBitTorrent m where
  liftBT :: BitTorrent a -> m a

newtype BitTorrent a = BitTorrent
  { unBitTorrent :: ReaderT Client (ResourceT IO) a
  } deriving (Monad, MonadIO)

instance MonadBitTorrent BitTorrent where
  liftBT = id

instance MonadDHT BitTorrent where
  liftDHT action = BitTorrent $ do
    node <- asks clientNode
    liftIO $ runResourceT $ runDHT node action

instance MonadLogger BitTorrent where
  monadLoggerLog loc src lvl msg = BitTorrent $ do
    logger <- asks clientLogger
    liftIO $ logger loc src lvl (toLogStr msg)

runBitTorrent :: Client -> BitTorrent a -> IO a
runBitTorrent client action = runResourceT $
  runReaderT (unBitTorrent action) client
{-# INLINE runBitTorrent #-}

getClient :: BitTorrent Client
getClient = BitTorrent ask

{-----------------------------------------------------------------------
--  Operations
-----------------------------------------------------------------------}
-- All operations should be non blocking!

addTorrent :: Torrent -> BitTorrent ()
addTorrent t = do
  Client {..} <- getClient
  liftIO $ do
    leecher <- newLeecher clientPeerId clientListenerPort t
    let ih = idInfoHash (tInfoDict t)
    atomically $ modifyTVar' clientTorrents (HM.insert ih leecher)
    askPeers leecher >>= print