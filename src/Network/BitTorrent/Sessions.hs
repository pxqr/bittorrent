-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Network.BitTorrent.Sessions
       ( -- * Progress
         Progress(..), startProgress
       , ClientService(..)
       , startService
       , withRunning

         -- * Client
       , ClientSession ( ClientSession
                       , clientPeerId, allowedExtensions
                       )
       , withClientSession

       , ThreadCount
       , defaultThreadCount

       , TorrentLoc(..)
       , registerTorrent
       , unregisterTorrent
       , getRegistered

       , getCurrentProgress
       , getSwarmCount
       , getPeerCount
       , getSwarm
       , getStorage
       , openSwarmSession

         -- * Swarm
       , SwarmSession( SwarmSession, torrentMeta, clientSession )

       , SessionCount
       , getSessionCount
       , getClientBitfield

       , discover
       ) where

import Prelude hiding (mapM_, elem)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MSem as MSem
import Control.Monad (forever, (>=>))
import Control.Exception
import Control.Monad.Trans

import Data.IORef
import Data.Map as M
import Data.HashMap.Strict as HM
import Data.Foldable as F
import Data.Set as S

import Network hiding (accept)
import Network.BSD
import Network.Socket

import Data.Bitfield as BF
import Data.Torrent
import Network.BitTorrent.Extension
import Network.BitTorrent.Peer
import Network.BitTorrent.Sessions.Types
import Network.BitTorrent.Exchange.Protocol as BT
import Network.BitTorrent.Tracker.Protocol as BT
import Network.BitTorrent.Tracker as BT
import Network.BitTorrent.Exchange as BT
import System.Torrent.Storage

{-----------------------------------------------------------------------
    Client Services
-----------------------------------------------------------------------}

startService :: MVar ClientService -> PortNumber -> (PortNumber -> IO ()) -> IO ()
startService s port m = do
    stopService s
    putMVar s =<< spawn
  where
    spawn = ClientService port <$> forkIO (m port)

stopService :: MVar ClientService -> IO ()
stopService = tryTakeMVar >=> maybe (return ()) (killThread . servThread)

-- | Service A might depend on service B.
withRunning :: MVar ClientService -> IO () -> (ClientService -> IO ()) -> IO ()
withRunning dep failure action = tryTakeMVar dep >>= maybe failure action

{-----------------------------------------------------------------------
    Torrent presence
-----------------------------------------------------------------------}

data TorrentPresence = Active     SwarmSession
                     | Registered TorrentLoc
                     | Unknown

torrentPresence :: ClientSession -> InfoHash -> IO TorrentPresence
torrentPresence ClientSession {..} ih = do
  sws <- readTVarIO swarmSessions
  case M.lookup ih sws of
    Just ss -> return $ Active ss
    Nothing -> do
      tm <- readTVarIO torrentMap
      return $ maybe Unknown Registered $ HM.lookup ih tm

{-----------------------------------------------------------------------
    Client sessions
-----------------------------------------------------------------------}

startListener :: ClientSession -> PortNumber -> IO ()
startListener cs @ ClientSession {..} port =
  startService peerListener port $ listener cs $ \conn @ (_, PeerSession{..}) -> do
      runP2P conn p2p

-- | Create a new client session. The data passed to this function are
-- usually loaded from configuration file.
openClientSession :: SessionCount -> [Extension] -> PortNumber -> PortNumber -> IO ClientSession
openClientSession n exts listenerPort _ = do
  cs <- ClientSession
    <$> genPeerId
    <*> pure exts
    <*> newEmptyMVar
    <*> MSem.new n
    <*> pure n
    <*> newTVarIO M.empty
    <*> newTVarIO (startProgress 0)
    <*> newTVarIO HM.empty

  startListener cs listenerPort
  return cs

closeClientSession :: ClientSession -> IO ()
closeClientSession ClientSession {..} = do
  stopService peerListener

  sws <- readTVarIO swarmSessions
  forM_ sws closeSwarmSession

withClientSession :: SessionCount -> [Extension]
                  -> PortNumber -> PortNumber
                  -> (ClientSession -> IO ()) -> IO ()
withClientSession c es l d = bracket (openClientSession c es l d) closeClientSession

-- | Get current global progress of the client. This value is usually
-- shown to a user.
getCurrentProgress :: MonadIO m => ClientSession -> m Progress
getCurrentProgress = liftIO . readTVarIO . currentProgress

-- | Get number of swarms client aware of.
getSwarmCount :: MonadIO m => ClientSession -> m SessionCount
getSwarmCount ClientSession {..} = liftIO $ M.size <$> readTVarIO swarmSessions

-- | Get number of peers the client currently connected to.
getPeerCount :: MonadIO m => ClientSession -> m ThreadCount
getPeerCount ClientSession {..} = liftIO $ do
  unused  <- peekAvail activeThreads
  return (maxActive - unused)

getListenerPort :: ClientSession -> IO PortNumber
getListenerPort ClientSession {..} = servPort <$> readMVar peerListener

{-----------------------------------------------------------------------
    Swarm session
-----------------------------------------------------------------------}

defSeederConns :: SessionCount
defSeederConns = defaultUnchokeSlots

defLeecherConns :: SessionCount
defLeecherConns = defaultNumWant

-- discovery should hide tracker and DHT communication under the hood
-- thus we can obtain an unified interface

discover :: SwarmSession -> IO ()
discover swarm @ SwarmSession {..} = {-# SCC discover #-} do
  port <- getListenerPort clientSession

  let conn = TConnection {
        tconnAnnounce = tAnnounce torrentMeta
      , tconnInfoHash = tInfoHash torrentMeta
      , tconnPeerId   = clientPeerId clientSession
      , tconnPort     = port
      }

  progress <- getCurrentProgress clientSession

  withTracker progress conn $ \tses -> do
    forever $ do
      addr <- getPeerAddr tses
      forkThrottle swarm $ do
        initiatePeerSession swarm addr $ \pconn -> do
          print addr
          runP2P pconn p2p

registerSwarmSession :: SwarmSession -> STM ()
registerSwarmSession ss @ SwarmSession {..} =
  modifyTVar' (swarmSessions clientSession) $
    M.insert (tInfoHash torrentMeta) ss

unregisterSwarmSession :: SwarmSession -> STM ()
unregisterSwarmSession SwarmSession {..} =
  modifyTVar' (swarmSessions clientSession) $
    M.delete $ tInfoHash torrentMeta

openSwarmSession :: ClientSession -> TorrentLoc -> IO SwarmSession
openSwarmSession cs @ ClientSession {..} loc @ TorrentLoc {..} = do
  t <- validateLocation loc
  let bf = haveNone $ pieceCount $ tInfo t

  ss <- SwarmSession t cs
    <$> MSem.new defLeecherConns
    <*> openStorage t dataDirPath bf
    <*> newTVarIO S.empty
    <*> newBroadcastTChanIO

  atomically $ do
    modifyTVar' currentProgress $ enqueuedProgress $ contentLength $ tInfo t
    registerSwarmSession ss

  _ <- forkIO $ discover ss

  return ss

closeSwarmSession :: SwarmSession -> IO ()
closeSwarmSession se @ SwarmSession {..} = do
  atomically $ unregisterSwarmSession se
  -- TODO stop discovery
  -- TODO killall peer sessions
  -- TODO the order is important!
  closeStorage storage

getSwarm :: ClientSession -> InfoHash -> IO SwarmSession
getSwarm cs @ ClientSession {..} ih = do
  tstatus <- torrentPresence cs ih
  case tstatus of
    Unknown        -> throw $ UnknownTorrent ih
    Active sw      -> return sw
    Registered loc -> openSwarmSession cs loc

-- TODO do not spawn session!
getStorage :: ClientSession -> InfoHash -> IO Storage
getStorage cs ih = storage <$> getSwarm cs ih

-- | Get the number of connected peers in the given swarm.
getSessionCount :: SwarmSession -> IO SessionCount
getSessionCount SwarmSession {..} = do
  S.size <$> readTVarIO connectedPeers

swarmHandshake :: SwarmSession   ->   Handshake
swarmHandshake    SwarmSession {..} = Handshake {
    hsProtocol = defaultBTProtocol
  , hsReserved = encodeExts $ allowedExtensions $ clientSession
  , hsInfoHash = tInfoHash torrentMeta
  , hsPeerId   = clientPeerId $ clientSession
  }

{-----------------------------------------------------------------------
    Peer sessions throttling
-----------------------------------------------------------------------}

-- | The number of threads suitable for a typical BT client.
defaultThreadCount :: ThreadCount
defaultThreadCount = 1000

enterSwarm :: SwarmSession -> IO ()
enterSwarm SwarmSession {..} = do
  MSem.wait (activeThreads clientSession)
  MSem.wait vacantPeers

leaveSwarm :: SwarmSession -> IO ()
leaveSwarm SwarmSession {..} = do
  MSem.signal vacantPeers
  MSem.signal (activeThreads clientSession)

forkThrottle :: SwarmSession -> IO () -> IO ThreadId
forkThrottle se action = do
   enterSwarm se
   (forkIO $ do
     action `finally` leaveSwarm se)
        `onException` leaveSwarm se

-- TODO: check content files location;
validateLocation :: TorrentLoc -> IO Torrent
validateLocation = fromFile . metafilePath

registerTorrent :: ClientSession -> TorrentLoc -> IO ()
registerTorrent ClientSession {..} loc @ TorrentLoc {..} = do
  torrent <- fromFile metafilePath
  atomically $ modifyTVar' torrentMap $ HM.insert (tInfoHash torrent) loc

unregisterTorrent :: TVar TorrentMap -> InfoHash -> IO ()
unregisterTorrent = error "unregisterTorrent"

getRegistered :: ClientSession -> IO TorrentMap
getRegistered ClientSession {..} = readTVarIO torrentMap

{-----------------------------------------------------------------------
  Peer session creation
------------------------------------------------------------------------
The peer session cycle looks like:

  * acquire vacant session and vacant thread slot;
  * (fork could be here, but not necessary)
  *   establish peer connection;
  *     register peer session;
  *       ... exchange process ...
  *     unregister peer session;
  *   close peer connection;
  * release acquired session and thread slot.

TODO: explain why this order
TODO: thread throttling
TODO: check if it connected yet peer
TODO: utilize peer Id.
TODO: use STM semaphore
-----------------------------------------------------------------------}

registerPeerSession :: PeerSession -> IO ()
registerPeerSession ps @ PeerSession {..}  =
  atomically $ modifyTVar' (connectedPeers swarmSession) (S.insert ps)

unregisterPeerSession :: PeerSession -> IO ()
unregisterPeerSession ps @ PeerSession {..} =
  atomically $ modifyTVar' (connectedPeers swarmSession) (S.delete ps)

openSession :: SwarmSession -> PeerAddr -> Handshake -> IO PeerSession
openSession ss @ SwarmSession {..} addr Handshake {..} = do
  let clientCaps = encodeExts $ allowedExtensions $ clientSession
  let enabled    = decodeExts (enabledCaps clientCaps hsReserved)

  bf <- getClientBitfield ss
  ps <- PeerSession addr ss enabled
    <$> atomically (dupTChan broadcastMessages)
    <*> newIORef (initialSessionState (totalCount bf))
    -- TODO we could implement more interesting throtling scheme
    -- using connected peer information
  registerPeerSession ps
  return ps

-- TODO kill thread
closeSession :: PeerSession -> IO ()
closeSession = unregisterPeerSession

type PeerConn = (Socket, PeerSession)
type Exchange = PeerConn -> IO ()

-- | Exchange action depends on session and socket, whereas session depends
--   on socket:
--
--   socket------>-----exchange
--     |                 |
--      \-->--session-->--/
--
--   To handle exceptions properly we double bracket socket and session
--   then joining the resources and also ignoring session local exceptions.
--
runSession :: IO Socket -> (Socket -> IO PeerSession) -> Exchange -> IO ()
runSession connector opener action =
  handle isSessionException $
    bracket connector close $ \sock ->
      bracket (opener sock) closeSession $ \ses ->
        action (sock, ses)

-- | Used then the client want to connect to a peer.
initiatePeerSession :: SwarmSession -> PeerAddr -> Exchange -> IO ()
initiatePeerSession ss @ SwarmSession {..} addr
    = runSession (connectToPeer addr) initiated
  where
    initiated sock = do
      phs  <- handshake sock (swarmHandshake ss)
      ps   <- openSession ss addr phs
      return ps

-- | Used the a peer want to connect to the client.
acceptPeerSession :: ClientSession -> PeerAddr -> Socket -> Exchange -> IO ()
acceptPeerSession cs@ClientSession {..} addr s = runSession (pure s) accepted
  where
    accepted sock = do
      phs   <- recvHandshake sock
      swarm <- getSwarm cs $ hsInfoHash phs
      ps    <- openSession swarm addr phs
      sendHandshake sock $ Handshake {
          hsProtocol = defaultBTProtocol
        , hsReserved = encodeExts $ enabledExtensions ps
        , hsInfoHash = hsInfoHash phs
        , hsPeerId   = clientPeerId
        }
      return ps

listener :: ClientSession -> Exchange -> PortNumber -> IO ()
listener cs action serverPort = bracket openListener close loop
  where
    loop sock = forever $ handle isIOError $ do
      (conn, addr) <- accept sock
      putStrLn "accepted"
      case addr of
        SockAddrInet port host -> do
          _ <- forkIO $ do
            acceptPeerSession cs (PeerAddr Nothing host port) conn action
          return ()
        _                      -> return ()

    isIOError :: IOError -> IO ()
    isIOError _ = return ()

    openListener  = do
      sock <- socket AF_INET Stream =<< getProtocolNumber "tcp"
      bindSocket sock (SockAddrInet serverPort iNADDR_ANY)
      listen sock maxListenQueue
      return sock
