{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BitTorrent.Client.Types
       ( -- * Core types
         Handle (..)
       , Client (..)

         -- * Monad BitTorrent
       , BitTorrent (..)
       , runBitTorrent
       , getClient

       , MonadBitTorrent (..)
       ) where

import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Function
import Data.HashMap.Strict as HM
import Data.Ord
import Network
import System.Log.FastLogger

import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.DHT      as DHT
import Network.BitTorrent.Exchange as Exchange
import Network.BitTorrent.Tracker  as Tracker

data Handle = Handle
  { topic    :: !InfoHash
  , private  :: !Bool
  , trackers :: !Tracker.Session
  , exchange :: !Exchange.Session
  }

data Client = Client
  { clientPeerId       :: !PeerId
  , clientListenerPort :: !PortNumber
  , allowedExtensions  :: !Caps
  , trackerManager     :: !Tracker.Manager
  , exchangeManager    :: !Exchange.Manager
  , clientNode         :: !(Node IPv4)
  , clientTorrents     :: !(MVar (HashMap InfoHash Handle))
  , clientLogger       :: !LogFun
  }

instance Eq Client where
  (==) = (==) `on` clientPeerId

instance Ord Client where
  compare = comparing clientPeerId

{-----------------------------------------------------------------------
--  BitTorrent monad
-----------------------------------------------------------------------}

newtype BitTorrent a = BitTorrent
  { unBitTorrent :: ReaderT Client (ResourceT IO) a
  } deriving (Functor, Monad, MonadIO)

class MonadBitTorrent m where
  liftBT :: BitTorrent a -> m a

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
