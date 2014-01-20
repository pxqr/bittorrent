{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
module Network.BitTorrent.DHT.Session
       ( -- * Options
         Alpha
       , defaultAlpha
       , K
       , defaultK
       , Options (..)

         -- * Session
       , DHT
       , runDHT
       , askOption

         -- * Tokens
       , grantToken
       , checkToken

         -- * Routing table
       , getTable
       , getNodeId
       , getClosest
       , insertNode

         -- * Peer storage
       , insertPeer
       , getPeerList
       , insertTopic
       , deleteTopic

         -- * Messaging
         -- ** Initiate
       , queryNode
       , queryParallel
       , (<@>)
       , ping

         -- ** Accept
       , NodeHandler
       , nodeHandler
       , pingH
       , findNodeH
       , getPeersH
       , announceH
       , handlers

         -- * Search
         -- ** Step
       , Iteration
       , findNodeQ
       , getPeersQ
       , announceQ

         -- ** Traversal
       , Search
       , search
       , publish
       ) where

import Prelude hiding (ioError)

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.Lifted hiding (yield)
import Control.Concurrent.Async.Lifted
import Control.Exception.Lifted hiding (Handler)
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.List as C hiding (mapMaybe, mapM_)
import Data.Default
import Data.Either
import Data.Fixed
import Data.Hashable
import Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Set as S
import Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Network (PortNumber)
import System.Log.FastLogger
import System.Random (randomIO)
import Text.PrettyPrint as PP hiding ((<>), ($$))
import Text.PrettyPrint.Class

import Data.Torrent.InfoHash
import Network.KRPC hiding (Options, def)
import qualified Network.KRPC as KRPC (def)
import Network.BitTorrent.Core
import Network.BitTorrent.Core.PeerAddr as P
import Network.BitTorrent.DHT.Message
import Network.BitTorrent.DHT.Routing as R
import Network.BitTorrent.DHT.Token   as T

{-----------------------------------------------------------------------
-- Options
-----------------------------------------------------------------------}

-- | Node lookups can proceed asynchronously.
type Alpha = Int

-- NOTE: libtorrent uses 5, azureus uses 10
-- | The quantity of simultaneous lookups is typically three.
defaultAlpha :: Alpha
defaultAlpha = 3

-- TODO add replication loop

-- TODO do not insert infohash -> peeraddr if infohash is too far from
-- this node id

data Order
  = NearFirst
  | FarFirst
  | Random

data Traversal
  = Greedy     -- ^ aggressive short-circuit traversal
  | Exhaustive -- ^

-- | Original Kamelia DHT uses term /publish/ for data replication
-- process. BitTorrent DHT uses term /announce/ since the purpose of
-- the DHT is peer discovery. Later in documentation, we use terms
-- /publish/ and /announce/ interchangible.
data Options = Options
  { -- | The degree of parallelism in 'find_node' queries. More
    -- parallism lead to faster bootstrapping and lookup operations,
    -- but also increase resource usage.
    --
    --   Normally this parameter should not exceed 'optK'.
    optAlpha       :: {-# UNPACK #-} !Alpha

    -- | /K/ parameter - number of nodes to return in 'find_node'
    -- responses.
  , optK           :: {-# UNPACK #-} !K

    -- | Number of buckets to maintain. This parameter depends on
    -- amount of nodes in the DHT network.
  , optBucketCount :: {-# UNPACK #-} !BucketCount

    -- | RPC timeout.
  , optTimeout     :: !NominalDiffTime

    -- | /R/ parameter - how many target nodes the 'announce' query
    -- should affect.
    --
    --   A large replica set compensates for inconsistent routing and
    --   reduces the need to frequently republish data for
    --   persistence. This comes at an increased cost for
    --   'Network.BitTorrent.DHT.insert' in terms of time, nodes
    --   contacted, and storage.
  , optReplication :: {-# UNPACK #-} !NodeCount

    -- | How often this node should republish (or reannounce) its
    -- data.
    --
    -- Large replica set ('optReplication') should require
    -- smaller reannounce intervals ('optReannounce').
  , optReannounce  :: !NominalDiffTime

    -- | The time it takes for data to expire in the
    --   network. Publisher of the data should republish (or
    --   reannounce) data to keep it in the network.
    --
    --   The /data expired timeout/ should be more than 'optReannounce'
    --   interval.
  , optDataExpired :: !NominalDiffTime
  } deriving (Show, Eq)

-- | Optimal options for bittorrent client. For short-lifetime
-- utilities you most likely need to tune 'optAlpha' and
-- 'optBucketCount'.
instance Default Options where
  def = Options
    { optAlpha       = defaultAlpha
    , optK           = defaultK

      -- see Fig.2 from "BitTorrent Mainline DHT Measurement" paper.
    , optBucketCount = defaultBucketCount

      -- see Fig.4 from "Profiling a Million User DHT" paper.
    , optTimeout     = 5  -- seconds
    , optReplication = 20 -- nodes
    , optReannounce  = 15 * 60
    , optDataExpired = 60 * 60
    }

seconds :: NominalDiffTime -> Int
seconds dt = fromEnum (realToFrac dt :: Uni)

{-----------------------------------------------------------------------
-- Tokens policy
-----------------------------------------------------------------------}

data SessionTokens = SessionTokens
  { tokenMap    :: !TokenMap
  , lastUpdate  :: !UTCTime
  , maxInterval :: !NominalDiffTime
  }

nullSessionTokens :: IO SessionTokens
nullSessionTokens = SessionTokens
  <$> (tokens <$> liftIO randomIO)
  <*> liftIO getCurrentTime
  <*> pure defaultUpdateInterval

invalidateTokens :: UTCTime -> SessionTokens -> SessionTokens
invalidateTokens curTime ts @ SessionTokens {..}
  | curTime `diffUTCTime` lastUpdate > maxInterval = SessionTokens
    { tokenMap    = update tokenMap
    , lastUpdate  = curTime
    , maxInterval = maxInterval
    }
  |                  otherwise                     = ts

{-----------------------------------------------------------------------
-- Session
-----------------------------------------------------------------------}

-- | A set of torrents this peer intends to share.
type AnnounceSet = Set (InfoHash, PortNumber)

-- | Logger function.
type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | DHT session keep track state of /this/ node.
data Node ip = Node
  { options       :: !Options                  -- ^ session configuration;
  , thisNodeId    :: !NodeId                   -- ^ session identifier;
  , manager       :: !(Manager (DHT       ip)) -- ^ RPC manager;
  , routingTable  :: !(MVar    (Table     ip)) -- ^ search table;
  , contactInfo   :: !(TVar    (PeerStore ip)) -- ^ published by other nodes;
  , announceInfo  :: !(TVar     AnnounceSet  ) -- ^ to publish by this node;
  , sessionTokens :: !(TVar     SessionTokens) -- ^ query session IDs.
  , loggerFun     :: !Logger
  }

-- | DHT keep track current session and proper resource allocation for
-- safe multithreading.
newtype DHT ip a = DHT { unDHT :: ReaderT (Node ip) (ResourceT IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadBase IO
           , MonadReader (Node ip)
           )

instance MonadBaseControl IO (DHT ip) where
  newtype StM (DHT ip) a = StM {
      unSt :: StM (ReaderT (Node ip) (ResourceT IO)) a
    }
  liftBaseWith cc = DHT $ liftBaseWith $ \ cc' ->
      cc $ \ (DHT m) -> StM <$> cc' m
  {-# INLINE liftBaseWith #-}

  restoreM = DHT . restoreM . unSt
  {-# INLINE restoreM #-}

instance MonadKRPC (DHT ip) (DHT ip) where
  getManager = asks manager

instance MonadLogger (DHT ip) where
  monadLoggerLog loc src lvl msg = do
    logger <- asks loggerFun
    liftIO $ logger loc src lvl (toLogStr msg)

-- | Run DHT session. Some resources like listener thread may live for
-- some short period of time right after this DHT session closed.
runDHT :: forall ip a. Address ip
       => [Handler (DHT ip)] -- ^ handlers to run on accepted queries;
       -> Options            -- ^ various dht options;
       -> NodeAddr ip        -- ^ node address to bind;
       -> DHT ip a           -- ^ DHT action to run;
       -> IO a               -- ^ result.
runDHT hs opts naddr action = runResourceT $ do
  runStderrLoggingT $ LoggingT $  \ logger -> do
    let rpcOpts  = KRPC.def { optQueryTimeout = seconds (optTimeout opts) }
    let nodeAddr = toSockAddr naddr
    (_, m) <- allocate (newManager rpcOpts nodeAddr hs) closeManager
    myId   <- liftIO genNodeId
    node   <- liftIO $ Node opts myId m
             <$> newMVar (nullTable myId (optBucketCount opts))
             <*> newTVarIO def
             <*> newTVarIO S.empty
             <*> (newTVarIO =<< nullSessionTokens)
             <*> pure logger
    runReaderT (unDHT (listen >> action)) node


askOption :: (Options -> a) -> DHT ip a
askOption f = asks (f . options)
{-# INLINE askOption #-}

{-----------------------------------------------------------------------
--  Routing
-----------------------------------------------------------------------}

routing :: Address ip => Routing ip a -> DHT ip (Maybe a)
routing = runRouting probeNode refreshNodes getTimestamp

probeNode :: Address ip => NodeAddr ip -> DHT ip Bool
probeNode addr = do
  $(logDebugS) "routing.questionable_node" (T.pack (render (pretty addr)))
  result <- try $ Ping <@> addr
  let _ = result :: Either SomeException Ping
  return $ either (const False) (const True) result

-- /pick a random ID/ in the range of the bucket and perform a
-- find_nodes search on it.

-- FIXME do not use getClosest sinse we should /refresh/ them
refreshNodes :: Address ip => NodeId -> DHT ip [NodeInfo ip]
refreshNodes nid = do
  $(logDebugS) "routing.refresh_bucket" (T.pack (render (pretty nid)))
  nodes <- getClosest nid
  nss <- forM (nodeAddr <$> nodes) $ \ addr -> do
    NodeFound ns <- FindNode nid <@> addr
    return ns
  return $ L.concat nss

getTimestamp :: DHT ip Timestamp
getTimestamp = do
  utcTime <- liftIO $ getCurrentTime
  $(logDebugS) "routing.make_timestamp" (T.pack (render (pretty utcTime)))
  return $ utcTimeToPOSIXSeconds utcTime

{-----------------------------------------------------------------------
-- Tokens
-----------------------------------------------------------------------}

tryUpdateSecret :: DHT ip ()
tryUpdateSecret = do
  curTime <- liftIO getCurrentTime
  toks    <- asks sessionTokens
  liftIO $ atomically $ modifyTVar' toks (invalidateTokens curTime)

grantToken :: Hashable a => NodeAddr a -> DHT ip Token
grantToken addr = do
  tryUpdateSecret
  toks <- asks sessionTokens >>= liftIO . readTVarIO
  return $ T.lookup addr $ tokenMap toks

-- | Throws 'HandlerError' if the token is invalid or already
-- expired. See 'TokenMap' for details.
checkToken :: Hashable a => NodeAddr a -> Token -> DHT ip ()
checkToken addr questionableToken = do
  tryUpdateSecret
  toks <- asks sessionTokens >>= liftIO . readTVarIO
  unless (T.member addr questionableToken (tokenMap toks)) $
    throwIO $ InvalidParameter "token"

{-----------------------------------------------------------------------
-- Routing table
-----------------------------------------------------------------------}

-- | Get current routing table. Normally you don't need to use this
-- function, but it can be usefull for debugging and profiling purposes.
getTable :: DHT ip (Table ip)
getTable = do
  var <- asks routingTable
  liftIO (readMVar var)

-- | Get id of /this/ node. This value is constant during DHT session.
getNodeId :: DHT ip NodeId
getNodeId = asks thisNodeId

-- | Find a set of closest nodes from routing table of this node. (in
-- no particular order)
--
--   This operation used for 'find_nodes' query.
--
getClosest :: Eq ip => TableKey k => k -> DHT ip [NodeInfo ip]
getClosest node = do
  k <- asks (optK . options)
  kclosest k node <$> getTable

-- | This operation do not block but acquire exclusive access to
--   routing table.
insertNode :: Address ip => NodeInfo ip -> DHT ip ThreadId
insertNode info = fork $ do
  var <- asks routingTable
  modifyMVar_ var $ \ t -> do
    result <- routing (R.insert info t)
    case result of
      Nothing -> do
        $(logDebugS) "insertNode" $ "Routing table is full: "
                   <> T.pack (show (pretty t))
        return t
      Just t' -> do
        let logMsg = "Routing table updated: "
                  <> pretty t <> " -> " <> pretty t'
        $(logDebugS) "insertNode" (T.pack (render logMsg))
        return t'

{-----------------------------------------------------------------------
-- Peer storage
-----------------------------------------------------------------------}

-- TODO limit dht peer store in size (probably by removing oldest peers)

-- | Insert peer to peer store. Used to handle announce requests.
insertPeer :: Eq ip => InfoHash -> PeerAddr ip -> DHT ip ()
insertPeer ih addr = do
  var <- asks contactInfo
  liftIO $ atomically $ modifyTVar' var (P.insert ih addr)

-- | Get peer set for specific swarm.
lookupPeers :: InfoHash -> DHT ip [PeerAddr ip]
lookupPeers ih = do
  var <- asks contactInfo
  liftIO $ P.lookup ih <$> readTVarIO var

-- | Prepare result for 'get_peers' query.
--
--   This operation use 'getClosest' as failback so it may block.
--
getPeerList :: Eq ip => InfoHash -> DHT ip (PeerList ip)
getPeerList ih = do
  ps <- lookupPeers ih
  if L.null ps
    then Left <$> getClosest ih
    else return (Right ps)

insertTopic :: InfoHash -> PortNumber -> DHT ip ()
insertTopic ih p = do
  var <- asks announceInfo
  liftIO $ atomically $ modifyTVar' var (S.insert (ih, p))

deleteTopic :: InfoHash -> PortNumber -> DHT ip ()
deleteTopic ih p = do
  var <- asks announceInfo
  liftIO $ atomically $ modifyTVar' var (S.delete (ih, p))

{-----------------------------------------------------------------------
-- Messaging
-----------------------------------------------------------------------}

-- | Throws exception if node is not responding.
queryNode :: forall a b ip. Address ip => KRPC (Query a) (Response b)
          => NodeAddr ip -> a -> DHT ip (NodeId, b)
queryNode addr q = do
  nid <- getNodeId
  Response remoteId r <- query (toSockAddr addr) (Query nid q)
  insertNode (NodeInfo remoteId addr)
  return (remoteId, r)

-- | Infix version of 'queryNode' function.
(<@>) :: Address ip => KRPC (Query a) (Response b)
      => a -> NodeAddr ip -> DHT ip b
q <@> addr = snd <$> queryNode addr q
{-# INLINE (<@>) #-}

-- TODO: use alpha
-- | Failed queries are ignored.
queryParallel :: [DHT ip a] -> DHT ip [a]
queryParallel queries = do
    alpha <- asks (optAlpha . options)
    cleanup <$> mapConcurrently try queries
  where
    cleanup :: [Either QueryFailure a] -> [a]
    cleanup = mapMaybe (either (const Nothing) Just)

ping :: Address ip => NodeAddr ip -> DHT ip (NodeInfo ip)
ping addr = do
  (nid, Ping) <- queryNode addr Ping
  return (NodeInfo nid addr)

type NodeHandler ip = Handler (DHT ip)

nodeHandler :: Address ip => KRPC (Query a) (Response b)
           => (NodeAddr ip -> a -> DHT ip b) -> NodeHandler ip
nodeHandler action = handler $ \ sockAddr (Query remoteId q) -> do
  case fromSockAddr sockAddr of
    Nothing    -> throwIO BadAddress
    Just naddr -> do
      insertNode (NodeInfo remoteId naddr)
      Response <$> getNodeId <*> action naddr q

{-----------------------------------------------------------------------
--  Search
-----------------------------------------------------------------------}

type Iteration ip o = NodeInfo ip -> DHT ip (Either [NodeInfo ip] [o ip])

-- TODO match with expected node id
findNodeQ :: Address ip => NodeId -> Iteration ip NodeInfo
findNodeQ nid NodeInfo {..} = do
  NodeFound closest <- FindNode nid <@> nodeAddr
  return $ Right closest

isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left  _) = True

getPeersQ :: Address ip => InfoHash -> Iteration ip PeerAddr
getPeersQ topic NodeInfo {..} = do
  GotPeers {..} <- GetPeers topic <@> nodeAddr
  let dist = distance (toNodeId topic) nodeId
  $(logInfoS) "getPeersQ" $ T.pack
         $ "distance: " <> render (pretty dist) <> " , result: "
        <> if isLeft peers then "NODES" else "PEERS"
  return peers

announceQ :: Address ip => InfoHash -> PortNumber -> Iteration ip NodeAddr
announceQ ih p NodeInfo {..} = do
  GotPeers {..} <- GetPeers ih <@> nodeAddr
  case peers of
    Left  ns
      | False     -> undefined -- TODO check if we can announce
      | otherwise -> return (Left ns)
    Right ps -> do -- TODO *probably* add to peer cache
      Announced <- Announce False ih p grantedToken <@> nodeAddr
      return (Right [nodeAddr])

type Search    ip o = Conduit [NodeInfo ip] (DHT ip) [o ip]

-- TODO: use reorder and filter (Traversal option) leftovers
search :: TableKey k => Address ip => k -> Iteration ip o -> Search ip o
search k action = do
  awaitForever $ \ batch -> unless (L.null batch) $ do
    $(logWarnS) "search" "start query"
    responses <- lift $ queryParallel (action <$> batch)
    let (nodes, results) = partitionEithers responses
    $(logWarnS) "search" "done query"
    leftover $ L.concat nodes
    mapM_ yield results

publish :: Address ip => InfoHash -> PortNumber -> DHT ip ()
publish ih p = do
  nodes <- getClosest ih
  r     <- asks (optReplication . options)
  _ <- sourceList [nodes] $= search ih (announceQ ih p) $$ C.take r
  return ()

republish :: DHT ip ThreadId
republish = fork $ do
  i <- askOption optReannounce
  error "DHT.republish: not implemented"

{-----------------------------------------------------------------------
--  Handlers
-----------------------------------------------------------------------}

pingH :: Address ip => NodeHandler ip
pingH = nodeHandler $ \ _ Ping -> do
  return Ping

findNodeH :: Address ip => NodeHandler ip
findNodeH = nodeHandler $ \ _ (FindNode nid) -> do
  NodeFound <$> getClosest nid

getPeersH :: Address ip => NodeHandler ip
getPeersH = nodeHandler $ \ naddr (GetPeers ih) -> do
  GotPeers <$> getPeerList ih <*> grantToken naddr

announceH :: Address ip => NodeHandler ip
announceH = nodeHandler $ \ naddr @ NodeAddr {..} (Announce {..}) -> do
  checkToken naddr sessionToken
  let annPort  = if impliedPort then nodePort else port
  let peerAddr = PeerAddr Nothing nodeHost annPort
  insertPeer topic peerAddr
  return Announced

handlers :: Address ip => [NodeHandler ip]
handlers = [pingH, findNodeH, getPeersH, announceH]
