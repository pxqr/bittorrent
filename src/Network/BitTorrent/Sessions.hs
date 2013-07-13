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

       , getCurrentProgress
       , getSwarmCount
       , getPeerCount

         -- * Swarm
       , SwarmSession( SwarmSession, torrentMeta, clientSession )

       , SessionCount
       , getSessionCount

       , newLeecher
       , newSeeder
       , getClientBitfield

       , discover
       ) where

import Prelude hiding (mapM_, elem)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MSem as MSem
import Control.Monad (when, forever, (>=>))
import Control.Exception
import Control.Monad.Trans

import Data.IORef
import Data.Map as M
import Data.HashMap.Strict as HM
import Data.Foldable as F
import Data.Set as S

import Data.Serialize hiding (get)

import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString

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
  startService peerListener port $ listener cs $ \conn @ (sock, PeerSession{..}) -> do
      print "accepted"
      let storage = error "storage"
      runP2P conn (exchange storage)

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

defLeacherConns :: SessionCount
defLeacherConns = defaultNumWant

-- discovery should hide tracker and DHT communication under the hood
-- thus we can obtain an unified interface

discover :: SwarmSession -> P2P () -> IO ()
discover swarm @ SwarmSession {..} action = {-# SCC discover #-} do
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
        initiatePeerSession swarm addr $ \conn ->
          runP2P conn action

registerSwarmSession :: SwarmSession -> IO ()
registerSwarmSession = undefined

unregisterSwarmSession :: SwarmSession -> IO ()
unregisterSwarmSession SwarmSession {..} =
  atomically $ modifyTVar (swarmSessions clientSession) $
    M.delete $ tInfoHash torrentMeta

newSwarmSession :: Int -> Bitfield -> ClientSession -> Torrent
                -> IO SwarmSession
newSwarmSession n bf cs @ ClientSession {..} t @ Torrent {..}
  = SwarmSession t cs
    <$> MSem.new n
    <*> newTVarIO bf
    <*> undefined
    <*> newTVarIO S.empty
    <*> newBroadcastTChanIO

-- > openSwarmSession :: ClientSession -> InfoHash -> IO SwarmSession
-- > openSwarmSession ClientSession {..} ih = do
-- >   loc <- HM.lookup <$> readTVarIO torrentMap
-- >   torrent <- validateLocation loc
-- >   return undefined

closeSwarmSession :: SwarmSession -> IO ()
closeSwarmSession se @ SwarmSession {..} = do
  unregisterSwarmSession se
  -- TODO stop discovery
  -- TODO killall peer sessions
  -- TODO the order is important!
  closeStorage storage

getSwarm :: ClientSession -> InfoHash -> IO SwarmSession
getSwarm cs @ ClientSession {..} ih = do
  ss <- readTVarIO swarmSessions
  case M.lookup ih ss of
    Just sw -> return sw
    Nothing -> undefined -- openSwarmSession cs

newSeeder :: ClientSession -> Torrent -> IO SwarmSession
newSeeder cs t @ Torrent {..}
  = newSwarmSession defSeederConns (haveAll (pieceCount tInfo)) cs t

-- | New swarm in which the client allowed both download and upload.
newLeecher :: ClientSession -> Torrent -> IO SwarmSession
newLeecher cs t @ Torrent {..} = do
  se <- newSwarmSession defLeacherConns (haveNone (pieceCount tInfo)) cs t
  atomically $ modifyTVar' (currentProgress cs) (enqueuedProgress (contentLength tInfo))
  return se

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

waitVacancy :: SwarmSession -> IO () -> IO ()
waitVacancy se = bracket (enterSwarm se) (const (leaveSwarm se)) . const

forkThrottle :: SwarmSession -> IO () -> IO ThreadId
forkThrottle se action = do
   enterSwarm se
   (forkIO $ do
     action `finally` leaveSwarm se)
        `onException` leaveSwarm se

-- TODO: check content files location;
validateLocation :: TorrentLoc -> IO Torrent
validateLocation = fromFile . metafilePath

registerTorrent :: TVar TorrentMap -> TorrentLoc -> IO ()
registerTorrent = error "registerTorrent"

unregisterTorrent :: TVar TorrentMap -> InfoHash -> IO ()
unregisterTorrent = error "unregisterTorrent"

torrentSwarm :: ClientSession -> InfoHash -> TorrentPresence -> IO SwarmSession
torrentSwarm _  _  (Active     sws) = return sws
torrentSwarm cs _  (Registered loc) = newSeeder cs =<< validateLocation loc
torrentSwarm _  ih  Unknown         = throw $ UnknownTorrent ih

lookupSwarm :: ClientSession -> InfoHash -> IO SwarmSession
lookupSwarm cs ih = torrentSwarm cs ih =<< torrentPresence cs ih

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
  ps <- PeerSession addr ss enabled
    <$> atomically (dupTChan broadcastMessages)
    <*> (newIORef . initialSessionState . totalCount =<< readTVarIO clientBitfield)
    -- TODO we could implement more interesting throtling scheme
    -- using connected peer information
  registerPeerSession ps
  return ps

closeSession :: PeerSession -> IO ()
closeSession = unregisterPeerSession

type PeerConn = (Socket, PeerSession)
type Exchange = PeerConn -> IO ()

sendClientStatus :: PeerConn -> IO ()
sendClientStatus (sock, PeerSession {..}) = do
  cbf <- readTVarIO $ clientBitfield $ swarmSession
  sendAll sock $ encode $ Bitfield cbf

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
      sendClientStatus (sock, ps)
      return ps

-- | Used the a peer want to connect to the client.
acceptPeerSession :: ClientSession -> PeerAddr -> Socket -> Exchange -> IO ()
acceptPeerSession cs@ClientSession {..} addr s = runSession (pure s) accepted
  where
    accepted sock = do
      phs   <- recvHandshake sock
      swarm <- lookupSwarm cs $ hsInfoHash phs
      ps    <- openSession swarm addr phs
      sendHandshake sock $ Handshake {
          hsProtocol = defaultBTProtocol
        , hsReserved = encodeExts $ enabledExtensions ps
        , hsInfoHash = hsInfoHash phs
        , hsPeerId   = clientPeerId
        }
      sendClientStatus (sock, ps)
      return ps

listener :: ClientSession -> Exchange -> PortNumber -> IO ()
listener cs action serverPort = bracket openListener close loop
  where
    loop sock = forever $ handle isIOError $ do
      (conn, addr) <- accept sock
      case addr of
        SockAddrInet port host -> do
          acceptPeerSession cs (PeerAddr Nothing host port) conn action
        _                      -> return ()

    isIOError :: IOError -> IO ()
    isIOError _ = return ()

    openListener  = do
      sock <- socket AF_INET Stream defaultProtocol
      bindSocket sock (SockAddrInet serverPort 0)
      listen sock 1
      return sock
