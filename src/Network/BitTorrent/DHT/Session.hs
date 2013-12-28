{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE RankNTypes #-} -- TODO remove
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
import Control.Exception hiding (Handler)
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Default
import Data.Hashable
import Data.List as L
import Data.Time
import Data.Time.Clock.POSIX
import System.Random (randomIO)

import Data.Torrent.InfoHash
import Network.KRPC
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
  , routingTable  :: !(TVar    (Table     ip))
  , contactInfo   :: !(TVar    (PeerStore ip))
  , sessionTokens :: !(TVar     SessionTokens)
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

runDHT :: forall ip a. Address ip
       => NodeAddr ip        -- ^ node address to bind;
       -> [Handler (DHT ip)] -- ^ handlers to run on accepted queries;
       -> DHT ip a           -- ^ DHT action to run;
       -> IO a               -- ^ result.
runDHT naddr handlers action = runResourceT $ do
  (_, m) <- allocate (newManager (toSockAddr naddr) handlers) closeManager
  myId   <- liftIO genNodeId
  node   <- liftIO $ Node m
             <$> newTVarIO (nullTable myId)
             <*> newTVarIO def
             <*> (newTVarIO =<< nullSessionTokens)
  runReaderT (unDHT (listen >> action)) node

{-----------------------------------------------------------------------
--  Routing
-----------------------------------------------------------------------}

--  TODO fork?
routing :: Address ip => Routing ip a -> DHT ip a
routing = runRouting ping refreshNodes getTimestamp

-- TODO add timeout
ping :: Address ip => NodeAddr ip -> DHT ip Bool
ping addr = do
  Ping <- Ping <@> addr
  return True

-- FIXME do not use getClosest sinse we should /refresh/ them
refreshNodes :: Address ip => NodeId -> DHT ip [NodeInfo ip]
refreshNodes nid = do
  nodes <- getClosest nid
  nss <- forM (nodeAddr <$> nodes) $ \ addr -> do
    NodeFound ns <- FindNode nid <@> addr
    return ns
  return $ L.concat nss

getTimestamp :: DHT ip Timestamp
getTimestamp = liftIO $ utcTimeToPOSIXSeconds <$> getCurrentTime

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
  liftIO (readTVarIO var)

putTable :: Table ip -> DHT ip ()
putTable table = do
  var <- asks routingTable
  liftIO (atomically (writeTVar var table))

getNodeId :: DHT ip NodeId
getNodeId = thisId <$> getTable

getClosest :: Eq ip => NodeId -> DHT ip [NodeInfo ip]
getClosest nid = kclosest 8 nid <$> getTable

getClosestHash :: Eq ip => InfoHash -> DHT ip [NodeInfo ip]
getClosestHash ih = kclosestHash 8 ih <$> getTable

insertNode :: Address ip => NodeInfo ip -> DHT ip ()
insertNode info = do
  t  <- getTable
  t' <- routing (R.insert info t)
  putTable t'

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

(<@>) :: Address ip => KRPC (Query a) (Response b)
      => a -> NodeAddr ip -> DHT ip b
q <@> addr = do
  nid <- getNodeId
  Response remoteId r <- query (toSockAddr addr) (Query nid q)
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
