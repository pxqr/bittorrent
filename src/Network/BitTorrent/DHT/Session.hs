{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
module Network.BitTorrent.DHT.Session
       ( -- * Session
         DHT
       , runDHT

         -- * Tokens
       , grantToken
       , checkToken

         -- * Routing table
       , getNodeId
       , getClosest
       , getClosestHash
       , insertNode

         -- * Peer storage
       , insertPeer
       , getPeerList

         -- * Messaging
       , (<@>)
       , NodeHandler
       , nodeHandler
       ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.Lifted
import Control.Exception.Lifted hiding (Handler)
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Default
import Data.Hashable
import Data.List as L
import Data.Monoid
import Data.Text as T
import Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX
import System.Log.FastLogger
import System.Random (randomIO)
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class

import Data.Torrent.InfoHash
import Network.KRPC
import Network.KRPC.Method
import Network.BitTorrent.Core
import Network.BitTorrent.Core.PeerAddr as P
import Network.BitTorrent.DHT.Message
import Network.BitTorrent.DHT.Routing as R
import Network.BitTorrent.DHT.Token   as T


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
  { manager       :: !(Manager (DHT       ip))
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
       => NodeAddr ip        -- ^ node address to bind;
       -> [Handler (DHT ip)] -- ^ handlers to run on accepted queries;
       -> DHT ip a           -- ^ DHT action to run;
       -> IO a               -- ^ result.
runDHT naddr handlers action = runResourceT $ do
  runStderrLoggingT $ LoggingT $  \ logger -> do
    (_, m) <- allocate (newManager (toSockAddr naddr) handlers) closeManager
    myId   <- liftIO genNodeId
    node   <- liftIO $ Node m
             <$> newMVar (nullTable myId)
             <*> newTVarIO def
             <*> (newTVarIO =<< nullSessionTokens)
             <*> pure logger
    runReaderT (unDHT (listen >> action)) node

{-----------------------------------------------------------------------
--  Routing
-----------------------------------------------------------------------}

--  TODO fork?
routing :: Address ip => Routing ip a -> DHT ip (Maybe a)
routing = runRouting ping refreshNodes getTimestamp

-- TODO add timeout
ping :: Address ip => NodeAddr ip -> DHT ip Bool
ping addr = do
  $(logDebugS) "routing.questionable_node" (T.pack (render (pretty addr)))
  result <- try $ Ping <@> addr
  let _ = result :: Either SomeException Ping
  return $ either (const False) (const True) result

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

-- | Throws 'ProtocolError' if token is invalid or already expired.
checkToken :: Hashable a => NodeAddr a -> Token -> DHT ip ()
checkToken addr questionableToken = do
  tryUpdateSecret
  toks <- asks sessionTokens >>= liftIO . readTVarIO
  unless (member addr questionableToken (tokenMap toks)) $
    liftIO $ throwIO $ KError ProtocolError "bad token" ""
     -- todo reset transaction id in krpc

{-----------------------------------------------------------------------
-- Routing table
-----------------------------------------------------------------------}

getTable :: DHT ip (Table ip)
getTable = do
  var <- asks routingTable
  liftIO (readMVar var)

getNodeId :: DHT ip NodeId
getNodeId = thisId <$> getTable

getClosest :: Eq ip => NodeId -> DHT ip [NodeInfo ip]
getClosest nid = kclosest 8 nid <$> getTable

getClosestHash :: Eq ip => InfoHash -> DHT ip [NodeInfo ip]
getClosestHash ih = kclosestHash 8 ih <$> getTable

-- FIXME some nodes can be ommited
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

(<@>) :: forall a b ip. Address ip => KRPC (Query a) (Response b)
      => a -> NodeAddr ip -> DHT ip b
q <@> addr = do
  nid <- getNodeId

  let Method name = method :: Method (Query a) (Response b)
  let signature = T.decodeUtf8 name <> " @ " <> T.pack (render (pretty addr))
  $(logDebugS) "queryNode" $ "Query sent | " <> signature
  Response remoteId r <- query (toSockAddr addr) (Query nid q)
  $(logDebugS) "queryNode" $ "Query recv | " <> signature

  insertNode (NodeInfo remoteId addr)
  return r

type NodeHandler ip = Handler (DHT ip)

nodeHandler :: Address ip => KRPC (Query a) (Response b)
           => (NodeAddr ip -> a -> DHT ip b) -> NodeHandler ip
nodeHandler action = handler $ \ sockAddr (Query remoteId q) -> do
  case fromSockAddr sockAddr of
    Nothing    -> liftIO $ throwIO $ KError GenericError "bad address" ""
    Just naddr -> do
      insertNode (NodeInfo remoteId naddr)
      Response <$> getNodeId <*> action naddr q
