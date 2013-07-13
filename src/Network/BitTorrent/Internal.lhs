> -- |
> --   Copyright   :  (c) Sam T. 2013
> --   License     :  MIT
> --   Maintainer  :  pxqr.sta@gmail.com
> --   Stability   :  experimental
> --   Portability :  portable
> --
> --   This module implement opaque broadcast message passing. It
> --   provides sessions needed by Network.BitTorrent and
> --   Network.BitTorrent.Exchange and modules. To hide some internals
> --   of this module we detach it from Exchange.
> --   NOTE: Expose only static data in data field lists, all dynamic
> --   data should be modified through standalone functions.
> --
>
> {-# LANGUAGE OverloadedStrings     #-}
> {-# LANGUAGE RecordWildCards       #-}
> {-# LANGUAGE ViewPatterns          #-}
> {-# LANGUAGE TemplateHaskell       #-}
> {-# LANGUAGE DeriveDataTypeable    #-}
>
> module Network.BitTorrent.Internal
>        ( -- * Progress
>          Progress(..), startProgress
>
>        , ClientService(..)
>
>          -- * Client
>        , ClientSession ( ClientSession
>                        , clientPeerId, allowedExtensions
>                        , nodeListener, peerListener
>                        )
>        , withClientSession
>        , listenerPort, dhtPort
>
>        , startService
>
>        , ThreadCount
>        , defaultThreadCount
>
>        , TorrentLoc(..)
>        , registerTorrent
>        , unregisterTorrent
>
>        , getCurrentProgress
>        , getSwarmCount
>        , getPeerCount
>
>          -- * Swarm
>        , SwarmSession( SwarmSession, torrentMeta, clientSession )
>
>        , SessionCount
>        , getSessionCount
>
>        , newLeecher
>        , newSeeder
>        , getClientBitfield
>
>        , enterSwarm
>        , leaveSwarm
>        , waitVacancy
>
>        , pieceLength
>
>          -- * Peer
>        , PeerSession( PeerSession, connectedPeerAddr
>                     , swarmSession, enabledExtensions
>                     , sessionState
>                     )
>        , SessionState
>        , initiatePeerSession
>        , acceptPeerSession
>        , listener
>
>          -- ** Broadcasting
>        , available
>        , getPending
>
>          -- ** Exceptions
>        , SessionException(..)
>        , isSessionException
>        , putSessionException
>
>          -- ** Properties
>        , bitfield, status
>        , findPieceCount
>
>          -- * Timeouts
>        , updateIncoming, updateOutcoming
>        ) where

> import Prelude hiding (mapM_)

> import Control.Applicative
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Concurrent.MSem as MSem
> import Control.Lens
> import Control.Monad (when, forever)
> import Control.Exception
> import Control.Monad.Trans

> import Data.IORef
> import Data.Default
> import Data.Function
> import Data.Foldable (mapM_)
> import Data.Map as M
> import Data.HashMap.Strict as HM
> import Data.Ord
> import Data.Set as S
> import Data.Typeable

> import Data.Serialize hiding (get)
> import Text.PrettyPrint

> import Network hiding (accept)
> import Network.Socket
> import Network.Socket.ByteString

> import GHC.Event as Ev

> import Data.Bitfield as BF
> import Data.Torrent
> import Network.BitTorrent.Extension
> import Network.BitTorrent.Peer
> import Network.BitTorrent.Exchange.Protocol as BT
> import Network.BitTorrent.Tracker.Protocol as BT
> import Network.BitTorrent.DHT.Protocol as BT

Progress
------------------------------------------------------------------------

Progress data is considered as dynamic within one client session. This
data also should be shared across client application sessions
(e.g. files), otherwise use 'startProgress' to get initial 'Progress'.

> -- | 'Progress' contains upload/download/left stats about
> --   current client state and used to notify the tracker.
> data Progress = Progress {
>     _uploaded   :: !Integer -- ^ Total amount of bytes uploaded.
>   , _downloaded :: !Integer -- ^ Total amount of bytes downloaded.
>   , _left       :: !Integer -- ^ Total amount of bytes left.
>   } deriving (Show, Read, Eq)
>
> $(makeLenses ''Progress)

**TODO:** Use Word64?

**TODO:** Use atomic bits?

Please note that tracker might penalize client some way if the do
not accumulate progress. If possible and save 'Progress' between
client sessions to avoid that.

> -- | Initial progress is used when there are no session before.
> startProgress :: Integer -> Progress
> startProgress = Progress 0 0

> -- | Used when the client download some data from /any/ peer.
> downloadedProgress :: Int -> Progress -> Progress
> downloadedProgress (fromIntegral -> amount)
>                  = (left         -~ amount)
>                  . (downloaded   +~ amount)
> {-# INLINE downloadedProgress #-}

> -- | Used when the client upload some data to /any/ peer.
> uploadedProgress :: Int -> Progress -> Progress
> uploadedProgress (fromIntegral -> amount) = uploaded +~ amount
> {-# INLINE uploadedProgress #-}

> -- | Used when leecher join client session.
> enqueuedProgress :: Integer -> Progress -> Progress
> enqueuedProgress amount = left +~ amount
> {-# INLINE enqueuedProgress #-}

> -- | Used when leecher leave client session.
> --   (e.g. user deletes not completed torrent)
> dequeuedProgress :: Integer -> Progress -> Progress
> dequeuedProgress amount = left -~ amount
> {-# INLINE dequeuedProgress #-}


Thread layout
------------------------------------------------------------------------

When client session created 2 new threads appear:

  * DHT listener - replies to DHT requests;

  * Peer listener - accept new P2P connection initiated by other
peers.

When swarn session created 3 new threads appear:

   * DHT request loop asks for new peers;

   * Tracker request loop asks for new peers;

   * controller which fork new avaand manage running P2P sessions.

Peer session is one always forked thread.

When client\/swarm\/peer session gets closed kill the corresponding
threads, but flush data to disc. (for e.g. storage block map)

So for e.g., in order to obtain our first block we need to spawn at
least 7 threads: main thread, 2 client session threads, 3 swarm session
threads and PeerSession thread.

Thread throttling
------------------------------------------------------------------------

If we will not restrict number of threads we could end up
with thousands of connected swarm and make no particular progress.

Note also we do not bound number of swarms! This is not optimal
strategy because each swarm might have say 1 thread and we could end
up bounded by the meaningless limit. Bounding global number of p2p
sessions should work better, and simpler.

**TODO:** priority based throttling: leecher thread have more priority
than seeder threads.

> -- | Each client might have a limited number of threads.
> type ThreadCount = Int

> -- | The number of threads suitable for a typical BT client.
> defaultThreadCount :: ThreadCount
> defaultThreadCount = 1000

Torrent Map
------------------------------------------------------------------------

TODO: keep track global peer have piece set.

Keeping all seeding torrent metafiles in memory is a _bad_ idea: for
1TB of data we need at least 100MB of metadata. (using 256KB piece
size). This solution do not scale further.

To avoid this we keep just *metainfo* about *metainfo*:

> -- | Local info about torrent location.
> data TorrentLoc = TorrentLoc {
>     -- | Full path to .torrent metafile.
>     metafilePath :: FilePath
>     -- | Full path to directory contating content files associated
>     -- with the metafile.
>   , dataDirPath  :: FilePath
>   }

TorrentMap is used to keep track all known torrents for the
client. When some peer trying to connect to us it's necessary to
dispatch appropriate 'SwarmSession' (or start new one if there are
none) in the listener loop: we only know 'InfoHash' from 'Handshake'
but nothing more. So to accept new 'PeerSession' we need to lookup
torrent metainfo and content files (if there are some) by the
'InfoHash' and only after that enter exchange loop.

TODO: check content files location;

> validateLocation :: TorrentLoc -> IO Torrent
> validateLocation = fromFile . metafilePath

Solution with TorrentLoc is much better and takes much more less
space, moreover it depends on count of torrents but not on count of
data itself. To scale further, in future we might add something like
database (for e.g. sqlite) for this kind of things.

> -- | Used to find torrent info and data in order to accept connection.
> type TorrentMap = HashMap InfoHash TorrentLoc

While *registering* torrent we need to check if torrent metafile is
correct, all the files are present in the filesystem and so
forth. However content validation using hashes will take a long time,
so we need to do this on demand: if a peer asks for a block, we
validate corresponding piece and only after read and send the block
back.

> registerTorrent :: TVar TorrentMap -> InfoHash -> TorrentLoc -> IO ()
> registerTorrent = error "registerTorrent"
>   {-
>   Torrent {..} <- validateTorrent tl
>   atomically $ modifyTVar' torrentMap $ HM.insert tInfoHash tl
>   return (Just t)
>   -}

> unregisterTorrent :: TVar TorrentMap -> InfoHash -> IO ()
> unregisterTorrent = error "unregisterTorrent"
>   -- modifyTVar' torrentMap $ HM.delete ih

Client Services
------------------------------------------------------------------------

There are two servers started as client start:

  * DHT node listener - needed by other peers to discover
  * Peer listener - need by other peers to join this client.

Thus any client (assuming DHT is enabled) provides at least 2 services
so we can abstract out into ClientService:

> data ClientService = ClientService {
>     servPort   :: !PortNumber
>   , servThread :: !ThreadId
>   } deriving Show

> startService :: PortNumber -> (PortNumber -> IO ()) -> IO ClientService
> startService port m = ClientService port <$> forkIO (m port)

> stopService :: ClientService -> IO ()
> stopService ClientService {..} = killThread servThread

Client Sessions
------------------------------------------------------------------------

Basically, client session should contain options which user
application store in configuration files and related to the
protocol. Moreover it should contain the all client identification
info, for e.g. DHT.

Client session is the basic unit of bittorrent network, it has:

  * The /peer ID/ used as unique identifier of the client in
network. Obviously, this value is not changed during client session.

  * The number of /protocol extensions/ it might use. This value is
static as well, but if you want to dynamically reconfigure the client
you might kill the end the current session and create a new with the
fresh required extensions.

  * The number of /swarms/ to join, each swarm described by the
'SwarmSession'.

Normally, you would have one client session, however, if we needed, in
one application we could have many clients with different peer ID's
and different enabled extensions at the same time.

> -- |
> data ClientSession = ClientSession {
>     -- | Used in handshakes and discovery mechanism.
>     clientPeerId      :: !PeerId

>     -- | Extensions we should try to use. Hovewer some particular peer
>     -- might not support some extension, so we keep enabledExtension in
>     -- 'PeerSession'.
>   , allowedExtensions :: [Extension]

>   , peerListener      :: !(MVar ClientService)
>   , nodeListener      :: !(MVar ClientService)

>     -- | Semaphor used to bound number of active P2P sessions.
>   , activeThreads     :: !(MSem ThreadCount)

>     -- | Max number of active connections.
>   , maxActive         :: !ThreadCount

>     -- | Used to traverse the swarm session.
>   , swarmSessions     :: !(TVar (Map InfoHash SwarmSession))

>   , eventManager      :: !EventManager

>     -- | Used to keep track global client progress.
>   , currentProgress   :: !(TVar  Progress)

>     -- | Used to keep track available torrents.
>   , torrentMap        :: !(TVar TorrentMap)
>   }

NOTE: currentProgress field is reduntant: progress depends on the all swarm
bitfields maybe we can remove the 'currentProgress' and compute it on
demand?

> instance Eq ClientSession where
>   (==) = (==) `on` clientPeerId

> instance Ord ClientSession where
>   compare = comparing clientPeerId

Torrent presence
------------------------------------------------------------------------

> data TorrentPresence = Active     SwarmSession
>                      | Registered TorrentLoc
>                      | Unknown

> torrentPresence :: ClientSession -> InfoHash -> IO TorrentPresence
> torrentPresence ClientSession {..} ih = do
>   sws <- readTVarIO swarmSessions
>   case M.lookup ih sws of
>     Just ss -> return $ Active ss
>     Nothing -> do
>       tm <- readTVarIO torrentMap
>       return $ maybe Unknown Registered $ HM.lookup ih tm

Retrieving client info
------------------------------------------------------------------------

> -- | Get current global progress of the client. This value is usually
> -- shown to a user.
> getCurrentProgress :: MonadIO m => ClientSession -> m Progress
> getCurrentProgress = liftIO . readTVarIO . currentProgress

> -- | Get number of swarms client aware of.
> getSwarmCount :: MonadIO m => ClientSession -> m SessionCount
> getSwarmCount ClientSession {..} = liftIO $
>   M.size <$> readTVarIO swarmSessions

> -- | Get number of peers the client currently connected to.
> getPeerCount :: MonadIO m => ClientSession -> m ThreadCount
> getPeerCount ClientSession {..} = liftIO $ do
>   unused  <- peekAvail activeThreads
>   return (maxActive - unused)

> -- | Create a new client session. The data passed to this function are
> -- usually loaded from configuration file.
> openClientSession :: SessionCount     -- ^ Maximum count of active P2P Sessions.
>           -> [Extension]      -- ^ Extensions allowed to use.
>           -> IO ClientSession -- ^ Client with unique peer ID.

> openClientSession n exts = do
>   mgr <- Ev.new
>   -- TODO kill this thread when leave client
>   _   <- forkIO $ loop mgr
>
>   ClientSession
>     <$> genPeerId
>     <*> pure exts
>     <*> newEmptyMVar
>     <*> newEmptyMVar
>     <*> MSem.new n
>     <*> pure n
>     <*> newTVarIO M.empty
>     <*> pure mgr
>     <*> newTVarIO (startProgress 0)
>     <*> newTVarIO HM.empty

> closeClientSession :: ClientSession -> IO ()
> closeClientSession ClientSession {..} =
>     maybeStop (tryTakeMVar peerListener) `finally`
>     maybeStop (tryTakeMVar nodeListener)
>   where
>     maybeStop m = maybe (return ()) stopService =<< m

> withClientSession :: SessionCount -> [Extension] -> (ClientSession -> IO ()) -> IO ()
> withClientSession c es = bracket (openClientSession c es) closeClientSession

> listenerPort :: ClientSession -> IO PortNumber
> listenerPort ClientSession {..} = servPort <$> readMVar peerListener

> dhtPort :: ClientSession -> IO PortNumber
> dhtPort ClientSession {..} = servPort <$> readMVar nodeListener

Swarm sessions
------------------------------------------------------------------------

NOTE: If client is a leecher then there is NO particular reason to
set max sessions count more than the_number_of_unchoke_slots * k:

  * thread slot(activeThread semaphore)
  * will take but no

So if client is a leecher then max sessions count depends on the
number of unchoke slots.

However if client is a seeder then the value depends on .

> -- | Used to bound the number of simultaneous connections and, which
> -- is the same, P2P sessions within the swarm session.
> type SessionCount = Int

> defSeederConns :: SessionCount
> defSeederConns = defaultUnchokeSlots

> defLeacherConns :: SessionCount
> defLeacherConns = defaultNumWant

> -- | Swarm session is
> data SwarmSession = SwarmSession {
>     torrentMeta       :: !Torrent

>   , clientSession     :: !ClientSession

TODO: lower "vacantPeers" when client becomes seeder according to
throttling policy.

Represent count of peers we _currently_ can connect to in the
swarm. Used to bound number of concurrent threads. See also *Thread
Throttling* section.

>   , vacantPeers       :: !(MSem SessionCount)

Client bitfield is used to keep track "the client have" piece set.
Modify this carefully always updating global progress.

>   , clientBitfield    :: !(TVar  Bitfield)

We keep set of the all connected peers for the each particular torrent
to prevent duplicated and therefore reduntant TCP connections. For
example consider the following very simle and realistic scenario:

 * Peer A lookup tracker for peers.

 * Peer B lookup tracker for peers.

 * Finally, Peer A connect to B and Peer B connect to peer A
simultaneously.

There some other situation the problem may occur: duplicates in
successive tracker responses, tracker and DHT returns. So without any
protection we end up with two session between the same peers. That's
bad because this could lead:

  * Reduced throughput - multiple sessions between the same peers will
mutiply control overhead (control messages, session state).

  * Thread occupation - duplicated sessions will eat thread slots and
discourage other, possible more useful, peers to establish connection.

To avoid this we could check, into the one transaction, if a peer is
already connected and add a connection only if it is not.

>   , connectedPeers    :: !(TVar (Set PeerSession))

TODO: use bounded broadcast chan with priority queue and drop old entries.

Channel used for replicate messages across all peers in swarm. For
exsample if we get some piece we should sent to all connected (and
interested in) peers HAVE message.

>   , broadcastMessages :: !(TChan Message)
>   }

INVARIANT: max_sessions_count - sizeof connectedPeers = value vacantPeers

> instance Eq SwarmSession where
>   (==) = (==) `on` (tInfoHash . torrentMeta)

> instance Ord SwarmSession where
>   compare = comparing (tInfoHash . torrentMeta)

> newSwarmSession :: Int -> Bitfield -> ClientSession -> Torrent
>                 -> IO SwarmSession
> newSwarmSession n bf cs @ ClientSession {..} t @ Torrent {..}
>   = SwarmSession <$> pure t
>                  <*> pure cs
>                  <*> MSem.new n
>                  <*> newTVarIO bf
>                  <*> newTVarIO S.empty
>                  <*> newBroadcastTChanIO

> -- | New swarm session in which the client allowed to upload only.
> newSeeder :: ClientSession -> Torrent -> IO SwarmSession
> newSeeder cs t @ Torrent {..}
>   = newSwarmSession defSeederConns (haveAll (pieceCount tInfo)) cs t

> -- | New swarm in which the client allowed both download and upload.
> newLeecher :: ClientSession -> Torrent -> IO SwarmSession
> newLeecher cs t @ Torrent {..} = do
>   se <- newSwarmSession defLeacherConns (haveNone (pieceCount tInfo)) cs t
>   atomically $ modifyTVar' (currentProgress cs) (enqueuedProgress (contentLength tInfo))
>   return se

> --isLeacher :: SwarmSession -> IO Bool
> --isLeacher = undefined

> -- | Get the number of connected peers in the given swarm.
> getSessionCount :: SwarmSession -> IO SessionCount
> getSessionCount SwarmSession {..} = do
>   S.size <$> readTVarIO connectedPeers

> getClientBitfield :: SwarmSession -> IO Bitfield
> getClientBitfield = readTVarIO . clientBitfield

> pieceLength :: SwarmSession -> Int
> pieceLength = ciPieceLength . tInfo . torrentMeta
> {-# INLINE pieceLength #-}

> swarmHandshake :: SwarmSession   ->   Handshake
> swarmHandshake    SwarmSession {..} = Handshake {
>     hsProtocol = defaultBTProtocol
>   , hsReserved = encodeExts $ allowedExtensions $ clientSession
>   , hsInfoHash = tInfoHash torrentMeta
>   , hsPeerId   = clientPeerId $ clientSession
>   }

> {-
> haveDone :: MonadIO m => PieceIx -> SwarmSession -> m ()
> haveDone ix =
>   liftIO $ atomically $ do
>     bf <- readTVar clientBitfield
>     writeTVar (have ix bf)
>     currentProgress
> -}

Peer sessions throttling
------------------------------------------------------------------------

> enterSwarm :: SwarmSession -> IO ()
> enterSwarm SwarmSession {..} = do
>   MSem.wait (activeThreads clientSession)
>   MSem.wait vacantPeers

> leaveSwarm :: SwarmSession -> IO ()
> leaveSwarm SwarmSession {..} = do
>   MSem.signal vacantPeers
>   MSem.signal (activeThreads clientSession)

> waitVacancy :: SwarmSession -> IO () -> IO ()
> waitVacancy se =
>   bracket (enterSwarm se) (const (leaveSwarm se))
>                   . const

Peer sessions
------------------------------------------------------------------------

> -- | Peer session contain all data necessary for peer to peer
> -- communication.
> data PeerSession = PeerSession {
>     -- | Used as unique 'PeerSession' identifier within one
>     -- 'SwarmSession'.
>     connectedPeerAddr :: !PeerAddr

>     -- | The swarm to which both end points belong to.
>   , swarmSession      :: !SwarmSession

>     -- | Extensions such that both peer and client support.
>   , enabledExtensions :: [Extension]

To dissconnect from died peers appropriately we should check if a peer
do not sent the KA message within given interval. If yes, we should
throw an exception in 'TimeoutCallback' and close session between
peers.

We should update timeout if we /receive/ any message within timeout
interval to keep connection up.

>   , incomingTimeout     :: !TimeoutKey

To send KA message appropriately we should know when was last time we
sent a message to a peer. To do that we keep registered timeout in
event manager and if we do not sent any message to the peer within
given interval then we send KA message in 'TimeoutCallback'.

We should update timeout if we /send/ any message within timeout to
avoid reduntant KA messages.

>   , outcomingTimeout   :: !TimeoutKey
>
>     -- | Broadcast messages waiting to be sent to peer.
>   , pendingMessages    :: !(TChan   Message)

>     -- | Dymanic P2P data.
>   , sessionState       :: !(IORef  SessionState)
>   }

> instance Eq PeerSession where
>   (==) = (==) `on` connectedPeerAddr

> instance Ord PeerSession where
>   compare = comparing connectedPeerAddr

Peer session state
------------------------------------------------------------------------

> data SessionState = SessionState {
>     _bitfield :: !Bitfield        -- ^ Other peer Have bitfield.
>   , _status   :: !SessionStatus   -- ^ Status of both peers.
>   } deriving (Show, Eq)

> $(makeLenses ''SessionState)

> initialSessionState :: PieceCount -> SessionState
> initialSessionState pc = SessionState (haveNone pc) def

> findPieceCount :: PeerSession -> PieceCount
> findPieceCount = pieceCount . tInfo . torrentMeta . swarmSession

Peer session exceptions
------------------------------------------------------------------------

> -- | Exceptions used to interrupt the current P2P session. This
> -- exceptions will NOT affect other P2P sessions, DHT, peer <->
> -- tracker, or any other session.
> --
> data SessionException = PeerDisconnected
>                       | ProtocolError  Doc
>                       | UnknownTorrent InfoHash
>                         deriving (Show, Typeable)

> instance Exception SessionException


> -- | Do nothing with exception, used with 'handle' or 'try'.
> isSessionException :: Monad m => SessionException -> m ()
> isSessionException _ = return ()

> -- | The same as 'isSessionException' but output to stdout the catched
> -- exception, for debugging purposes only.
> putSessionException :: SessionException -> IO ()
> putSessionException = print

> torrentSwarm :: ClientSession -> InfoHash -> TorrentPresence -> IO SwarmSession
> torrentSwarm _  _  (Active     sws) = return sws
> torrentSwarm cs _  (Registered loc) = newSeeder cs =<< validateLocation loc
> torrentSwarm _  ih  Unknown         = throw $ UnknownTorrent ih

> lookupSwarm :: ClientSession -> InfoHash -> IO SwarmSession
> lookupSwarm cs ih = torrentSwarm cs ih =<< torrentPresence cs ih

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

> openSession :: SwarmSession -> PeerAddr -> Handshake -> IO PeerSession
> openSession ss @ SwarmSession {..} addr Handshake {..} = do
>   let clientCaps = encodeExts $ allowedExtensions $ clientSession
>   let enabled    = decodeExts (enabledCaps clientCaps hsReserved)
>   ps <- PeerSession addr ss enabled
>     <$> registerTimeout (eventManager clientSession) maxIncomingTime (return ())
>     <*> registerTimeout (eventManager clientSession) maxOutcomingTime (return ())
>     <*> atomically (dupTChan broadcastMessages)
>     <*> (newIORef . initialSessionState . totalCount =<< readTVarIO clientBitfield)
>   -- TODO we could implement more interesting throtling scheme
>   -- using connected peer information
>   atomically $ modifyTVar' connectedPeers (S.insert ps)
>   return ps

> closeSession :: PeerSession -> IO ()
> closeSession ps @ PeerSession {..} = do
>   atomically $ modifyTVar' (connectedPeers swarmSession) (S.delete ps)

> type PeerConn = (Socket, PeerSession)
> type Exchange = PeerConn -> IO ()

> sendClientStatus :: PeerConn -> IO ()
> sendClientStatus (sock, PeerSession {..}) = do
>   cbf <- readTVarIO $ clientBitfield $ swarmSession
>   sendAll sock $ encode $ Bitfield cbf
>
>   port <- dhtPort $ clientSession swarmSession
>   when (ExtDHT `elem` enabledExtensions) $ do
>     sendAll sock $ encode $ Port port

Exchange action depends on session and socket, whereas session depends
on socket:

  socket------>-----exchange
     |                 |
     \-->--session-->--/

To handle exceptions properly we double bracket socket and session
then joining the resources and also ignoring session local exceptions.

> runSession :: IO Socket -> (Socket -> IO PeerSession) -> Exchange -> IO ()
> runSession connector opener action =
>   handle isSessionException $
>     bracket connector close $ \sock ->
>       bracket (opener sock) closeSession $ \ses ->
>         action (sock, ses)

Used then the client want to connect to a peer.

> initiatePeerSession :: SwarmSession -> PeerAddr -> Exchange -> IO ()
> initiatePeerSession ss @ SwarmSession {..} addr
>     = runSession (connectToPeer addr) initiated
>   where
>     initiated sock = do
>       phs  <- handshake sock (swarmHandshake ss)
>       ps   <- openSession ss addr phs
>       sendClientStatus (sock, ps)
>       return ps

Used the a peer want to connect to the client.

> acceptPeerSession :: ClientSession -> PeerAddr -> Socket -> Exchange -> IO ()
> acceptPeerSession cs@ClientSession {..} addr s = runSession (pure s) accepted
>   where
>     accepted sock = do
>       phs   <- recvHandshake sock
>       swarm <- lookupSwarm cs $ hsInfoHash phs
>       ps    <- openSession swarm addr phs
>       sendHandshake sock $ Handshake {
>           hsProtocol = defaultBTProtocol
>         , hsReserved = encodeExts $ enabledExtensions ps
>         , hsInfoHash = hsInfoHash phs
>         , hsPeerId   = clientPeerId
>         }
>       sendClientStatus (sock, ps)
>       return ps


> listener :: ClientSession -> Exchange -> PortNumber -> IO ()
> listener cs action serverPort = bracket openListener close loop
>   where
>    loop sock = forever $ handle isIOError $ do
>      (conn, addr) <- accept sock
>      case addr of
>        SockAddrInet port host -> do
>          acceptPeerSession cs (PeerAddr Nothing host port) conn action
>        _                      -> return ()
>
>    isIOError :: IOError -> IO ()
>    isIOError _ = return ()
>
>    openListener  = do
>      sock <- socket AF_INET Stream defaultProtocol
>      bindSocket sock (SockAddrInet serverPort 0)
>      listen sock 1
>      return sock

Broadcasting: Have, Cancel, Bitfield, SuggestPiece
------------------------------------------------------------------------

Here we should enqueue broadcast messages and keep in mind that:
  * We should enqueue broadcast events as they are appear.
  * We should yield broadcast messages as fast as we get them.

these 2 phases might differ in time significantly

**TODO**: do this; but only when it'll be clean which other broadcast
messages & events we should send.

1. Update client have bitfield --\____ in one transaction;
2. Update downloaded stats     --/
3. Signal to the all other peer about this.

> available :: Bitfield -> SwarmSession -> IO ()
> available bf se @ SwarmSession {..} = {-# SCC available #-} do
>     mark >> atomically broadcast
>   where
>     mark = do
>       let bytes = pieceLength se * BF.haveCount bf
>       atomically $ do
>         modifyTVar' clientBitfield (BF.union bf)
>         modifyTVar' (currentProgress clientSession) (downloadedProgress bytes)
>
>     broadcast = mapM_ (writeTChan broadcastMessages . Have) (BF.toList bf)


TODO compute size of messages: if it's faster to send Bitfield
instead many Have do that

Also if there is single Have message in queue then the
corresponding piece is likely still in memory or disc cache,
when we can send SuggestPiece.

Get pending messages queue appeared in result of asynchronously
changed client state. Resulting queue should be sent to a peer
immediately.

> getPending :: PeerSession -> IO [Message]
> getPending PeerSession {..} = {-# SCC getPending #-} do
>   atomically (readAvail pendingMessages)

> readAvail :: TChan a -> STM [a]
> readAvail chan = do
>   m <- tryReadTChan chan
>   case m of
>     Just a  -> (:) <$> pure a <*> readAvail chan
>     Nothing -> return []

Timeouts
-----------------------------------------------------------------------

for internal use only

> sec :: Int
> sec = 1000 * 1000

> maxIncomingTime :: Int
> maxIncomingTime = 120 * sec

> maxOutcomingTime :: Int
> maxOutcomingTime = 1 * sec

> -- | Should be called after we have received any message from a peer.
> updateIncoming :: PeerSession -> IO ()
> updateIncoming PeerSession {..} = do
>   updateTimeout (eventManager (clientSession swarmSession))
>     incomingTimeout maxIncomingTime

> -- | Should be called before we have send any message to a peer.
> updateOutcoming :: PeerSession -> IO ()
> updateOutcoming PeerSession {..}  =
>   updateTimeout (eventManager (clientSession swarmSession))
>     outcomingTimeout maxOutcomingTime

> sendKA :: Socket -> IO ()
> sendKA sock {- SwarmSession {..} -} = do
>   return ()
> --  print "I'm sending keep alive."
> --  sendAll sock (encode BT.KeepAlive)
> --  let mgr = eventManager clientSession
> --  updateTimeout mgr
> --  print "Done.."
