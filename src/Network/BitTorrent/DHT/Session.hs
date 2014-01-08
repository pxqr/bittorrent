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

         -- * Tokens
       , grantToken
       , checkToken

         -- * Routing table
       , getTable
       , getNodeId
       , getClosest
       , getClosestHash
       , insertNode

         -- * Peer storage
       , insertPeer
       , getPeerList

         -- * Messaging
         -- ** Initiate
       , queryNode
       , (<@>)

         -- ** Accept
       , NodeHandler
       , nodeHandler

         -- ** Iterate
       , Iteration
       , Search
       , search
       ) where

import Prelude hiding (ioError)

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.Lifted hiding (yield)
import Control.Exception.Lifted hiding (Handler)
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Default
import Data.Fixed
import Data.Hashable
import Data.List as L
import Data.Monoid
import Data.Text as T
import Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX
import System.Log.FastLogger
import System.Random (randomIO)
import System.Timeout.Lifted
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class

import Data.Torrent.InfoHash
import Network.KRPC hiding (Options, def)
import qualified Network.KRPC as KRPC (Options, def)
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

-- | The quantity of simultaneous lookups is typically three.
defaultAlpha :: Alpha
defaultAlpha = 3

data Options = Options
  { -- | the degree of parallelism in 'find_node' queries.
    optAlpha       :: {-# UNPACK #-} !Alpha

    -- | number of nodes to return in 'find_node' responses.
  , optK           :: {-# UNPACK #-} !K

    -- | Number of buckets to maintain.
  , optBucketCount :: {-# UNPACK #-} !BucketCount

    -- | RPC timeout.
  , optTimeout     ::                !NominalDiffTime

--  , optReannounceInterval :: NominalDiffTime
--  , optDataExpiredTimeout :: NominalDiffTime
  } deriving (Show, Eq)

instance Default Options where
  def = Options
    { optAlpha       = defaultAlpha
    , optK           = defaultK
    , optBucketCount = defaultBucketCount
    , optTimeout     = 5 -- seconds
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

data Node ip = Node
  { options       :: !Options
  , manager       :: !(Manager (DHT       ip))
  , routingTable  :: !(MVar    (Table     ip))
  , contactInfo   :: !(TVar    (PeerStore ip))
  , sessionTokens :: !(TVar     SessionTokens)
  , loggerFun     :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  }

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

runDHT :: forall ip a. Address ip
       => [Handler (DHT ip)] -- ^ handlers to run on accepted queries;
       -> Options            -- ^ various dht options;
       -> NodeAddr ip        -- ^ node address to bind;
       -> DHT ip a           -- ^ DHT action to run;
       -> IO a               -- ^ result.
runDHT handlers opts naddr action = runResourceT $ do
  runStderrLoggingT $ LoggingT $  \ logger -> do
    let rpcOpts  = KRPC.def { optQueryTimeout = seconds (optTimeout opts) }
    let nodeAddr = toSockAddr naddr
    (_, m) <- allocate (newManager rpcOpts nodeAddr handlers) closeManager
    myId   <- liftIO genNodeId
    node   <- liftIO $ Node opts m
             <$> newMVar (nullTable myId (optBucketCount opts))
             <*> newTVarIO def
             <*> (newTVarIO =<< nullSessionTokens)
             <*> pure logger
    runReaderT (unDHT (listen >> action)) node

{-----------------------------------------------------------------------
--  Routing
-----------------------------------------------------------------------}

routing :: Address ip => Routing ip a -> DHT ip (Maybe a)
routing = runRouting ping refreshNodes getTimestamp

ping :: Address ip => NodeAddr ip -> DHT ip Bool
ping addr = do
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
  unless (member addr questionableToken (tokenMap toks)) $
    throwIO $ InvalidParameter "token"

{-----------------------------------------------------------------------
-- Routing table
-----------------------------------------------------------------------}

getTable :: DHT ip (Table ip)
getTable = do
  var <- asks routingTable
  liftIO (readMVar var)

-- FIXME no blocking
getNodeId :: DHT ip NodeId
getNodeId = thisId <$> getTable

getClosest :: Eq ip => NodeId -> DHT ip [NodeInfo ip]
getClosest nid = do
  k <- asks (optK . options)
  kclosest k nid <$> getTable

getClosestHash :: Eq ip => InfoHash -> DHT ip [NodeInfo ip]
getClosestHash ih = do
  k <- asks (optK . options)
  kclosestHash k ih <$> getTable

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
        let logMsg = "Routing table updated: " <> pretty t <> " -> " <> pretty t'
        $(logDebugS) "insertNode" (T.pack (render logMsg))
        return t'

{-----------------------------------------------------------------------
-- Peer storage
-----------------------------------------------------------------------}

insertPeer :: Eq ip => InfoHash -> PeerAddr ip -> DHT ip ()
insertPeer ih addr = do
  var <- asks contactInfo
  liftIO $ atomically $ modifyTVar' var (P.insert ih addr)

lookupPeers :: InfoHash -> DHT ip [PeerAddr ip]
lookupPeers ih = do
  var <- asks contactInfo
  liftIO $ P.lookup ih <$> readTVarIO var

type PeerList ip = Either [NodeInfo ip] [PeerAddr ip]

getPeerList :: Eq ip => InfoHash -> DHT ip (PeerList ip)
getPeerList ih = do
  ps <- lookupPeers ih
  if L.null ps
    then Left <$> getClosestHash ih
    else return (Right ps)

{-----------------------------------------------------------------------
-- Messaging
-----------------------------------------------------------------------}

-- | Throws exception if node is not responding.
queryNode :: forall a b ip. Address ip => KRPC (Query a) (Response b)
          => NodeAddr ip -> a -> DHT ip b
queryNode addr q = do
  nid <- getNodeId
  Response remoteId r <- query (toSockAddr addr) (Query nid q)
  insertNode (NodeInfo remoteId addr)
  return r

-- | Infix version of 'queryNode' function.
(<@>) :: Address ip => KRPC (Query a) (Response b)
      => a -> NodeAddr ip -> DHT ip b
(<@>) = flip queryNode
{-# INLINE (<@>) #-}

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

type Iteration ip i o = i ip -> DHT ip (Either [i ip] [o ip])
type Search    ip i o = Conduit [i ip] (DHT ip) [o ip]

-- TODO: use all inputs
search :: Address ip => Iteration ip i o -> Search ip i o
search action = do
  alpha <- lift $ asks (optAlpha . options)
  awaitForever $ \ inputs -> do
    forM_ (L.take alpha inputs) $ \ input -> do
      result <- lift $ try $ action input
      case result of
        Left  e -> let _ = e :: IOError in return ()
        Right r -> either leftover yield r
