-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module implement opaque broadcast message passing. It
--   provides sessions needed by Network.BitTorrent and
--   Network.BitTorrent.Exchange and modules. To hide some internals
--   of this module we detach it from Exchange.
--
--   Note: expose only static data in data field lists, all dynamic
--   data should be modified through standalone functions.
--
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Network.BitTorrent.Internal
       ( Progress(..), startProgress

         -- * Client
       , ClientSession (clientPeerID, allowedExtensions)
       , newClient, getCurrentProgress

         -- * Swarm
       , SwarmSession(SwarmSession, torrentMeta, clientSession)
       , newLeacher, newSeeder
       , enterSwarm, leaveSwarm , waitVacancy

         -- * Peer
       , PeerSession(PeerSession, connectedPeerAddr
                    , swarmSession, enabledExtensions
                    )
       , SessionState
       , withPeerSession

         -- ** Exceptions
       , SessionException(..)
       , isSessionException
       , putSessionException
       , sessionError

         -- ** Properties
       , bitfield, status
       , emptyBF, fullBF, singletonBF, adjustBF
       , getPieceCount, getClientBF

         -- * Timeouts
       , updateIncoming, updateOutcoming
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MSem as MSem
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception

import Data.IORef
import Data.Default
import Data.Function
import Data.Ord
import Data.Set as S
import Data.Typeable

import Data.Serialize hiding (get)
import Text.PrettyPrint

import Network
import Network.Socket
import Network.Socket.ByteString

import GHC.Event as Ev

import Data.Bitfield as BF
import Data.Torrent
import Network.BitTorrent.Extension
import Network.BitTorrent.Peer
import Network.BitTorrent.Exchange.Protocol as BT
import Network.BitTorrent.Tracker.Protocol as BT


-- | 'Progress' contains upload/download/left stats about
--   current client state.
--
--   This data is considered as dynamic within one session.
--
data Progress = Progress {
    prUploaded   :: Integer -- ^ Total amount of bytes uploaded.
  , prDownloaded :: Integer -- ^ Total amount of bytes downloaded.
  , prLeft       :: Integer -- ^ Total amount of bytes left.
  } deriving Show

startProgress :: Integer -> Progress
startProgress = Progress 0 0


{-----------------------------------------------------------------------
    Client session
-----------------------------------------------------------------------}

type ThreadCount = Int

-- | In one application we could have many clients with difference
-- ID's and different enabled extensions.
data ClientSession = ClientSession {
    -- | Our peer ID used in handshaked and discovery mechanism. The
    -- clientPeerID is unique 'ClientSession' identifier.
    clientPeerID      :: PeerID

    -- | Extensions we should try to use. Hovewer some particular peer
    -- might not support some extension, so we keep enableExtension in
    -- 'PeerSession'.
  , allowedExtensions :: [Extension]

    -- | Semaphor used to bound number of active P2P sessions.
  , activeThreads     :: MSem ThreadCount

    -- | Max number of active connections.
  , maxActive         :: ThreadCount

  , swarmSessions     :: TVar (Set SwarmSession)

  , eventManager      :: EventManager
  , currentProgress   :: TVar  Progress
  }

instance Eq ClientSession where
  (==) = (==) `on` clientPeerID

instance Ord ClientSession where
  compare = comparing clientPeerID

getCurrentProgress :: MonadIO m => ClientSession -> m Progress
getCurrentProgress = liftIO . readTVarIO . currentProgress

newClient :: ThreadCount      -- ^ Maximum count of active P2P Sessions.
          -> [Extension]      -- ^ Extensions allowed to use.
          -> IO ClientSession

newClient n exts = do
  mgr <- Ev.new
  -- TODO kill this thread when leave client
  _   <- forkIO $ loop mgr

  ClientSession
    <$> newPeerID
    <*> pure exts
    <*> MSem.new n
    <*> pure n
    <*> newTVarIO S.empty
    <*> pure mgr
    <*> newTVarIO (startProgress 0)

{-----------------------------------------------------------------------
    Swarm session
-----------------------------------------------------------------------}

type SessionCount = Int

-- | Extensions are set globally by
--   Swarm session are un
data SwarmSession = SwarmSession {
    torrentMeta       :: Torrent
  , clientSession     :: ClientSession

    -- | Represent count of peers we _currently_ can connect to in the
    -- swarm. Used to bound number of concurrent threads.
  , vacantPeers       :: MSem SessionCount

    -- | Modify this carefully updating global progress.
  , clientBitfield    :: TVar  Bitfield
  , connectedPeers    :: TVar (Set PeerSession)
  }

instance Eq SwarmSession where
  (==) = (==) `on` (tInfoHash . torrentMeta)

instance Ord SwarmSession where
  compare = comparing (tInfoHash . torrentMeta)

newSwarmSession :: Int -> Bitfield -> ClientSession -> Torrent
                -> IO SwarmSession
newSwarmSession n bf cs @ ClientSession {..} t @ Torrent {..}
  = SwarmSession <$> pure t
                 <*> pure cs
                 <*> MSem.new n
                 <*> newTVarIO bf
                 <*> newTVarIO S.empty

newSeeder :: ClientSession -> Torrent -> IO SwarmSession
newSeeder cs t @ Torrent {..}
  = newSwarmSession defSeederConns (haveAll (pieceCount tInfo)) cs t

newLeacher :: ClientSession -> Torrent -> IO SwarmSession
newLeacher cs t @ Torrent {..}
  = newSwarmSession defLeacherConns (haveNone (pieceCount tInfo)) cs t

defSeederConns :: SessionCount
defSeederConns = defaultUnchokeSlots

defLeacherConns :: SessionCount
defLeacherConns = defaultNumWant

--isLeacher :: SwarmSession -> IO Bool
--isLeacher = undefined

{-
haveDone :: MonadIO m => PieceIx -> SwarmSession -> m ()
haveDone ix =
  liftIO $ atomically $ do
    bf <- readTVar clientBitfield
    writeTVar (have ix bf)
    currentProgress
-}

enterSwarm :: SwarmSession -> IO ()
enterSwarm SwarmSession {..} = do
  MSem.wait (activeThreads clientSession)
  MSem.wait vacantPeers

leaveSwarm :: SwarmSession -> IO ()
leaveSwarm SwarmSession {..} = do
  MSem.signal vacantPeers
  MSem.signal (activeThreads clientSession)

waitVacancy :: SwarmSession -> IO () -> IO ()
waitVacancy se =
  bracket (enterSwarm se) (const (leaveSwarm se))
                  . const

{-----------------------------------------------------------------------
    Peer session
-----------------------------------------------------------------------}

data PeerSession = PeerSession {
    -- | Used as unique 'PeerSession' identifier within one
    -- 'SwarmSession'.
    connectedPeerAddr :: PeerAddr

  , swarmSession      :: SwarmSession

    -- | Extensions such that both peer and client support.
  , enabledExtensions :: [Extension]

    -- | To dissconnect from died peers appropriately we should check
    -- if a peer do not sent the KA message within given interval. If
    -- yes, we should throw an exception in 'TimeoutCallback' and
    -- close session between peers.
    --
    -- We should update timeout if we /receive/ any message within
    -- timeout interval to keep connection up.
  , incomingTimeout     :: TimeoutKey

    -- | To send KA message appropriately we should know when was last
    -- time we sent a message to a peer. To do that we keep registered
    -- timeout in event manager and if we do not sent any message to
    -- the peer within given interval then we send KA message in
    -- 'TimeoutCallback'.
    --
    -- We should update timeout if we /send/ any message within timeout
    -- to avoid reduntant KA messages.
  , outcomingTimeout   :: TimeoutKey

    -- TODO use dupChan for broadcasting
  , broadcastMessages :: Chan   [Message]
  , sessionState      :: IORef   SessionState
  }

data SessionState = SessionState {
    _bitfield :: Bitfield
  , _status   :: SessionStatus
  }

$(makeLenses ''SessionState)

instance Eq PeerSession where
  (==) = (==) `on` connectedPeerAddr

instance Ord PeerSession where
  compare = comparing connectedPeerAddr

instance (MonadIO m, MonadReader PeerSession m)
      => MonadState SessionState m where
  get   = asks sessionState >>= liftIO . readIORef
  put s = asks sessionState >>= \ref -> liftIO $ writeIORef ref s

data SessionException = SessionException
                        deriving (Show, Typeable)

instance Exception SessionException

isSessionException :: Monad m => SessionException -> m ()
isSessionException _ = return ()

putSessionException :: SessionException -> IO ()
putSessionException = print

sessionError :: MonadIO m => Doc -> m ()
sessionError msg
  = liftIO $ throwIO $ userError $ render $ msg <+> "in session"

-- TODO check if it connected yet peer
withPeerSession :: SwarmSession -> PeerAddr
                -> ((Socket, PeerSession) -> IO ())
                -> IO ()

withPeerSession ss @ SwarmSession {..} addr
    = handle isSessionException . bracket openSession closeSession
  where
    openSession = do
      let caps  = encodeExts $ allowedExtensions $ clientSession
      let ihash = tInfoHash torrentMeta
      let pid   = clientPeerID $ clientSession
      let chs   = Handshake defaultBTProtocol caps ihash pid

      sock <- connectToPeer addr
      phs  <- handshake sock chs `onException` close sock

      cbf <- readTVarIO clientBitfield
      sendAll sock (encode (Bitfield cbf))

      let enabled = decodeExts (enabledCaps caps (handshakeCaps phs))
      ps <- PeerSession addr ss enabled
         <$> registerTimeout (eventManager clientSession)
                maxIncomingTime abortSession
         <*> registerTimeout (eventManager clientSession)
                maxOutcomingTime (sendKA sock)
         <*> newChan
         <*> do {
           ; tc <- totalCount <$> readTVarIO clientBitfield
           ; newIORef (SessionState (haveNone tc) def)
           }
      return (sock, ps)

    closeSession (sock, _) = do
      close sock

getPieceCount :: (MonadReader PeerSession m) => m PieceCount
getPieceCount = asks (pieceCount . tInfo . torrentMeta . swarmSession)

emptyBF :: (MonadReader PeerSession m) => m Bitfield
emptyBF = liftM haveNone getPieceCount

fullBF ::  (MonadReader PeerSession m) => m Bitfield
fullBF = liftM haveAll getPieceCount

singletonBF :: (MonadReader PeerSession m) => PieceIx -> m Bitfield
singletonBF i = liftM (BF.singleton i) getPieceCount

adjustBF :: (MonadReader PeerSession m) => Bitfield -> m Bitfield
adjustBF bf = (`adjustSize` bf) `liftM` getPieceCount

getClientBF :: (MonadIO m, MonadReader PeerSession m) => m Bitfield
getClientBF = asks swarmSession >>= liftIO . readTVarIO . clientBitfield

--data Signal =
--nextBroadcast :: P2P (Maybe Signal)
--nextBroadcast =


{-----------------------------------------------------------------------
    Timeouts
-----------------------------------------------------------------------}

sec :: Int
sec = 1000 * 1000

maxIncomingTime :: Int
maxIncomingTime = 120 * sec

maxOutcomingTime :: Int
maxOutcomingTime = 1 * sec

-- | Should be called after we have received any message from a peer.
updateIncoming :: PeerSession -> IO ()
updateIncoming PeerSession {..} = do
  updateTimeout (eventManager (clientSession swarmSession))
    incomingTimeout maxIncomingTime

-- | Should be called before we have send any message to a peer.
updateOutcoming :: PeerSession -> IO ()
updateOutcoming PeerSession {..}  =
  updateTimeout (eventManager (clientSession swarmSession))
    outcomingTimeout maxOutcomingTime

sendKA :: Socket -> IO ()
sendKA sock {- SwarmSession {..} -} = do
  return ()
--  print "I'm sending keep alive."
--  sendAll sock (encode BT.KeepAlive)
--  let mgr = eventManager clientSession
--  updateTimeout mgr
--  print "Done.."

abortSession :: IO ()
abortSession = error "abortSession: not implemented"
