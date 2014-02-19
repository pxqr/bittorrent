-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   BitTorrent uses a \"distributed sloppy hash table\" (DHT) for
--   storing peer contact information for \"trackerless\" torrents. In
--   effect, each peer becomes a tracker.
--
--   Normally you don't need to import other DHT modules.
--
--   For more info see:
--   <http://www.bittorrent.org/beps/bep_0005.html>
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Network.BitTorrent.DHT
       ( -- * Distributed Hash Table
         DHT
       , Options (..)
       , dht

         -- * Bootstrapping
       , tNodes
       , defaultBootstrapNodes
       , resolveHostName
       , bootstrap
       , isBootstrapped

         -- * Initialization
       , snapshot
       , restore

         -- * Operations
       , Network.BitTorrent.DHT.lookup
       , Network.BitTorrent.DHT.insert
       , Network.BitTorrent.DHT.delete

         -- * Embedding
         -- ** Session
       , LogFun
       , Node
       , handlers
       , startNode

         -- ** Monad
       , MonadDHT (..)
       , runDHT
       ) where

import Control.Applicative
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Exception
import Data.ByteString as BS
import Data.Conduit as C
import Data.Conduit.List as C
import Network.Socket

import Data.Torrent (tNodes)
import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Query
import Network.BitTorrent.DHT.Session
import Network.BitTorrent.DHT.Routing as T

{-----------------------------------------------------------------------
--  DHT types
-----------------------------------------------------------------------}

class MonadDHT m where
  liftDHT :: DHT IPv4 a -> m a

instance MonadDHT (DHT IPv4) where
  liftDHT = id

-- | Run DHT on specified port. <add note about resources>
dht :: Address ip
    => Options     -- ^ normally you need to use 'Data.Default.def';
    -> NodeAddr ip -- ^ address to bind this node;
    -> DHT ip a    -- ^ actions to run: 'bootstrap', 'lookup', etc;
    -> IO a        -- ^ result.
dht opts addr action = do
  runStderrLoggingT $ LoggingT $ \ logger -> do
    bracket (startNode handlers opts addr logger) stopNode $ \ node ->
      runDHT node action
{-# INLINE dht #-}

{-----------------------------------------------------------------------
--  Bootstrapping
-----------------------------------------------------------------------}
-- Do not include the following hosts in the default bootstrap nodes list:
--
--   * "dht.aelitis.com" and "dht6.azureusplatform.com" - since
--   Azureus client have a different (and probably incompatible) DHT
--   protocol implementation.
--
--   * "router.utorrent.com" since it is just an alias to
--   "router.bittorrent.com".

-- | List of bootstrap nodes maintained by different bittorrent
-- software authors.
defaultBootstrapNodes :: [NodeAddr HostName]
defaultBootstrapNodes =
  [ NodeAddr "router.bittorrent.com"  6881 -- by BitTorrent Inc.

    -- doesn't work at the moment (use git blame)  of commit
  , NodeAddr "dht.transmissionbt.com" 6881 -- by Transmission project
  ]

-- TODO Multihomed hosts

-- | Resolve either a numeric network address or a hostname to a
-- numeric IP address of the node.  Usually used to resolve
-- 'defaultBootstrapNodes' or 'Data.Torrent.tNodes' lists.
resolveHostName :: NodeAddr HostName -> IO (NodeAddr IPv4)
resolveHostName NodeAddr {..} = do
  let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Datagram }
  -- getAddrInfo throws exception on empty list, so the pattern matching never fail
  info : _ <- getAddrInfo (Just hints) (Just nodeHost) (Just (show nodePort))
  case fromSockAddr (addrAddress info) of
    Nothing   -> error "resolveNodeAddr: impossible"
    Just addr -> return addr

-- | One good node may be sufficient.
--
--   This operation do block, use
--   'Control.Concurrent.Async.Lifted.async' if needed.
--
bootstrap :: Address ip => [NodeAddr ip] -> DHT ip ()
bootstrap startNodes = do
  $(logInfoS) "bootstrap" "Start node bootstrapping"
  nid <- asks thisNodeId
  -- TODO filter duplicated in startNodes list
  aliveNodes <- queryParallel (pingQ <$> startNodes)
  _ <- sourceList [aliveNodes] $= search nid (findNodeQ nid) $$ C.consume
  $(logInfoS) "bootstrap" "Node bootstrapping finished"

-- | Check if this node is already bootstrapped.
--   @bootstrap [good_node] >> isBootstrapped@@ should always return 'True'.
--
--   This operation do not block.
--
isBootstrapped :: DHT ip Bool
isBootstrapped = T.full <$> getTable

{-----------------------------------------------------------------------
-- Initialization
-----------------------------------------------------------------------}

-- | Load previous session. (corrupted - exception/ignore ?)
--
--   This is blocking operation, use
--   'Control.Concurrent.Async.Lifted.async' if needed.
restore :: ByteString -> DHT ip ()
restore = error "DHT.restore: not implemented"

-- | Serialize current DHT session to byte string.
--
--   This is blocking operation, use
-- 'Control.Concurrent.Async.Lifted.async' if needed.
snapshot :: DHT ip ByteString
snapshot = error "DHT.snapshot: not implemented"

{-----------------------------------------------------------------------
--  Operations
-----------------------------------------------------------------------}

-- | Get list of peers which downloading this torrent.
--
--   This operation is incremental and do block.
--
lookup :: Address ip => InfoHash -> DHT ip `Source` [PeerAddr ip]
lookup topic = do      -- TODO retry getClosest if bucket is empty
  closest <- lift $ getClosest topic
  sourceList [closest] $= search topic (getPeersQ topic)

-- | Announce that /this/ peer may have some pieces of the specified
-- torrent. DHT will reannounce this data periodically using
-- 'optReannounce' interval.
--
--   This operation is synchronous and do block, use
--   'Control.Concurrent.Async.Lifted.async' if needed.
--
insert :: Address ip => InfoHash -> PortNumber -> DHT ip ()
insert ih p = do
  publish ih p
  insertTopic ih p

-- | Stop announcing /this/ peer for the specified torrent.
--
--   This operation is atomic and may block for a while.
--
delete :: Address ip => InfoHash -> PortNumber -> DHT ip ()
delete = deleteTopic
{-# INLINE delete #-}
