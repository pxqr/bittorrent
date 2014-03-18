{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BitTorrent.Client.Types
       ( -- * Core types
         Handle (..)
       , Client (..)
       , externalAddr

         -- * Monad BitTorrent
       , BitTorrent (..)
       , runBitTorrent
       , getClient

       , MonadBitTorrent (..)
       ) where

import Control.Applicative
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
  , clientResources    :: !InternalState
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

-- | External IP address of a host running a bittorrent client
-- software may be used to acknowledge remote peer the host connected
-- to. See 'Network.BitTorrent.Exchange.Message.ExtendedHandshake'.
externalAddr :: Client -> PeerAddr (Maybe IP)
externalAddr Client {..} = PeerAddr
  { peerId   = Just clientPeerId
  , peerHost = Nothing -- TODO return external IP address, if known
  , peerPort = clientListenerPort
  }

{-----------------------------------------------------------------------
--  BitTorrent monad
-----------------------------------------------------------------------}

newtype BitTorrent a = BitTorrent
  { unBitTorrent :: ReaderT Client IO a
  } deriving ( Functor, Applicative, Monad
             , MonadIO, MonadThrow, MonadUnsafeIO
             )

class MonadBitTorrent m where
  liftBT :: BitTorrent a -> m a

-- | NOP.
instance MonadBitTorrent BitTorrent where
  liftBT = id

instance MonadTrans t => MonadBitTorrent (t BitTorrent) where
  liftBT = lift

-- | Registered but not closed manually resources will be
-- automatically closed at 'Network.BitTorrent.Client.closeClient'
instance MonadResource BitTorrent where
  liftResourceT m = BitTorrent $ do
    s <- asks clientResources
    liftIO $ runInternalState m s

-- | Run DHT operation, only if the client node is running.
instance MonadDHT BitTorrent where
  liftDHT action = BitTorrent $ do
    node <- asks clientNode
    liftIO $ runDHT node action

instance MonadLogger BitTorrent where
  monadLoggerLog loc src lvl msg = BitTorrent $ do
    logger <- asks clientLogger
    liftIO $ logger loc src lvl (toLogStr msg)

runBitTorrent :: Client -> BitTorrent a -> IO a
runBitTorrent client action = runReaderT (unBitTorrent action) client
{-# INLINE runBitTorrent #-}

getClient :: BitTorrent Client
getClient = BitTorrent ask
{-# INLINE getClient #-}