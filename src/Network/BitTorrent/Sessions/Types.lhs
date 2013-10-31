> -- |
> --   Copyright   :  (c) Sam Truzjan 2013
> --   License     :  BSD3
> --   Maintainer  :  pxqr.sta@gmail.com
> --   Stability   :  experimental
> --   Portability :  portable
> --
>
> {-# LANGUAGE RecordWildCards       #-}
> {-# LANGUAGE ViewPatterns          #-}
> {-# LANGUAGE TemplateHaskell       #-}
> {-# LANGUAGE DeriveDataTypeable    #-}
>
> module Network.BitTorrent.Sessions.Types
>        ( ClientService(..)
>        , ThreadCount
>        , TorrentLoc (..)
>        , TorrentMap
>
>        , ClientSession (..)
>
>        , SwarmSession (..)
>        , getClientBitfield
>        , getPending, available
>
>        , PeerSession (..)
>        , SessionCount
>        , findPieceCount
>
>        , SessionState (..)
>        , status, bitfield
>        , initialSessionState
>        , getSessionState
>
>        , SessionException (..)
>        , isSessionException, putSessionException
>        ) where

> import Control.Applicative
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Concurrent.MSem as MSem
> import Control.Lens
> import Control.Exception

> import Data.IORef
> import Data.Default
> import Data.Function
> import Data.Map as M
> import Data.HashMap.Strict as HM
> import Data.Ord
> import Data.Set as S
> import Data.Typeable
> import Text.PrettyPrint

> import Network

> import Data.Torrent.Bitfield as BF
> import Network.BitTorrent.Extension
> import Network.BitTorrent.Peer
> import Network.BitTorrent.Exchange.Protocol as BT
> import System.Torrent.Storage

Thread layout
------------------------------------------------------------------------

When client session created 2 new threads appear:

  * Peer listener - accept new P2P connection initiated by other
peers;

  * Tracker announcer - announce that the peer have this torrent.

  * OPTIONAL: DHT listener - replies to DHT requests;

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
>   } deriving Show

TorrentMap is used to keep track all known torrents for the
client. When some peer trying to connect to us it's necessary to
dispatch appropriate 'SwarmSession' (or start new one if there are
none) in the listener loop: we only know 'InfoHash' from 'Handshake'
but nothing more. So to accept new 'PeerSession' we need to lookup
torrent metainfo and content files (if there are some) by the
'InfoHash' and only after that enter exchange loop.

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

>     -- | Semaphor used to bound number of active P2P sessions.
>   , activeThreads     :: !(MSem ThreadCount)

>     -- | Max number of active connections.
>   , maxActive         :: !ThreadCount

>     -- | Used to traverse the swarm session.
>   , swarmSessions     :: !(TVar (Map InfoHash SwarmSession))

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

Swarm sessions
------------------------------------------------------------------------

NOTE: If client is a leecher then there is NO particular reason to
set max sessions count more than the_number_of_unchoke_slots * k:

  * thread slot(activeThread semaphore)
  * will take but no

So if client is a leecher then max sessions count depends on the
number of unchoke slots.

> -- | Used to bound the number of simultaneous connections and, which
> --   is the same, P2P sessions within the swarm session.
> type SessionCount = Int

However if client is a seeder then the value depends on .

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

>   , storage           :: !Storage

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

> getClientBitfield :: SwarmSession -> IO Bitfield
> getClientBitfield SwarmSession {..} = atomically $ getCompleteBitfield storage

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

>     -- | Broadcast messages waiting to be sent to peer.
>   , pendingMessages    :: !(TChan   Message)

>     -- | Dymanic P2P data.
>   , sessionState       :: !(IORef  SessionState)
>   }

> instance Eq PeerSession where
>   (==) = (==) `on` connectedPeerAddr

> instance Ord PeerSession where
>   compare = comparing connectedPeerAddr

> findPieceCount :: PeerSession -> PieceCount
> findPieceCount = pieceCount . tInfo . torrentMeta . swarmSession

Peer session state
------------------------------------------------------------------------

> data SessionState = SessionState {
>     _bitfield :: !Bitfield        -- ^ Other peer Have bitfield.
>   , _status   :: !SessionStatus   -- ^ Status of both peers.
>   } deriving (Show, Eq)

> $(makeLenses ''SessionState)

> initialSessionState :: PieceCount -> SessionState
> initialSessionState pc = SessionState (haveNone pc) def

> getSessionState :: PeerSession -> IO SessionState
> getSessionState PeerSession {..} = readIORef sessionState

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

> available :: Bitfield -> SwarmSession -> STM ()
> available bf SwarmSession {..} = {-# SCC available #-} do
>     updateProgress >> broadcast
>   where
>     updateProgress = do
>       let piLen = ciPieceLength $ tInfo $ torrentMeta
>       let bytes = piLen * BF.haveCount bf
>       modifyTVar' (currentProgress clientSession) (downloadedProgress bytes)
>
>     broadcast = mapM_ (writeTChan broadcastMessages . Have) (BF.toList bf)

-- TODO compute size of messages: if it's faster to send Bitfield
-- instead many Have do that

-- Also if there is single Have message in queue then the
-- corresponding piece is likely still in memory or disc cache,
-- when we can send SuggestPiece.

-- | Get pending messages queue appeared in result of asynchronously
--   changed client state. Resulting queue should be sent to a peer
-- immediately.

> getPending :: PeerSession -> IO [Message]
> getPending PeerSession {..} = {-# SCC getPending #-} do
>  atomically (readAvail pendingMessages)

> readAvail :: TChan a -> STM [a]
> readAvail chan = do
>   m <- tryReadTChan chan
>   case m of
>     Just a  -> (:) <$> pure a <*> readAvail chan
>     Nothing -> return []
