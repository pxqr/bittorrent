{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BitTorrent.Client.Types
       ( -- * Core types
         HandleStatus (..)
       , Handle (..)
       , Client (..)
       , externalAddr

         -- * Monad BitTorrent
       , BitTorrent (..)
       , runBitTorrent
       , getClient

       , MonadBitTorrent (..)

         -- * Events
       , Types.Event (..)
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan.Split as CS
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Function
import Data.HashMap.Strict as HM
import Data.Ord
import Network
import System.Log.FastLogger

import Data.Torrent.InfoHash
import Network.BitTorrent.Internal.Types as Types
import Network.BitTorrent.Core
import Network.BitTorrent.DHT      as DHT
import Network.BitTorrent.Exchange as Exchange
import Network.BitTorrent.Tracker  as Tracker hiding (Event)

data HandleStatus
  = Running
  | Stopped
    deriving (Show, Eq)

data Handle = Handle
  { handleTopic    :: !InfoHash
  , handlePrivate  :: !Bool

  , handleStatus   :: !(MVar HandleStatus)
  , handleTrackers :: !Tracker.Session
  , handleExchange :: !Exchange.Session
  , handleEvents   :: !(SendPort (Event Handle))
  }

instance EventSource Handle where
  data Event Handle  = StatusChanged HandleStatus
  listen Handle {..} = CS.listen undefined

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
  , clientEvents       :: !(SendPort (Event Client))
  }

instance Eq Client where
  (==) = (==) `on` clientPeerId

instance Ord Client where
  compare = comparing clientPeerId

instance EventSource Client where
  data Event Client = TorrentAdded InfoHash
  listen Client {..} = CS.listen clientEvents

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
             , MonadIO, MonadThrow, MonadUnsafeIO, MonadBase IO
             )

class MonadBitTorrent m where
  liftBT :: BitTorrent a -> m a

instance MonadBaseControl IO BitTorrent where
  newtype StM BitTorrent a = StM { unSt :: StM (ReaderT Client IO) a }
  liftBaseWith cc = BitTorrent $ liftBaseWith $ \ cc' ->
      cc $ \ (BitTorrent m) -> StM <$> cc' m
  {-# INLINE liftBaseWith #-}

  restoreM = BitTorrent . restoreM . unSt
  {-# INLINE restoreM #-}

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