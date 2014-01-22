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
       , MonadDHT (..)
       , dht

         -- * Initialization
       , bootstrap
       , snapshot
       , restore

         -- * Operations
       , Network.BitTorrent.DHT.lookup
       , Network.BitTorrent.DHT.insert
       , Network.BitTorrent.DHT.delete

         -- * Internal
         -- | Can be used to implement instance of 'MonadDHT'.
       , LogFun
       , Node
       , handlers
       , startNode
       , runDHT
       ) where

import Control.Applicative
import Control.Monad.Logger
import Control.Monad.Trans
import Data.ByteString as BS
import Data.Conduit as C
import Data.Conduit.List as C
import Network.Socket (PortNumber)

import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Session

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
  runResourceT $ do
    runStderrLoggingT $ LoggingT $ \ logger -> do
      node <- startNode handlers opts addr logger
      runDHT node action
{-# INLINE dht #-}

{-----------------------------------------------------------------------
--  Initialization
-----------------------------------------------------------------------}

-- | One good node may be sufficient. The list of bootstrapping nodes
-- usually obtained from 'Data.Torrent.tNodes' field. Bootstrapping
-- process can take up to 5 minutes.
--
--   This operation is synchronous and do block, use
--   'Control.Concurrent.Async.Lifted.async' if needed.
--
bootstrap :: Address ip => [NodeAddr ip] -> DHT ip ()
bootstrap startNodes = do
  $(logInfoS) "bootstrap" "Start node bootstrapping"
  nid <- getNodeId
  aliveNodes <- queryParallel (ping <$> startNodes)
  _ <- sourceList [aliveNodes] $= search nid (findNodeQ nid) $$ C.consume
  $(logInfoS) "bootstrap" "Node bootstrapping finished"
--     t <- getTable
--     unless (full t) $ do
--      nid <- getNodeId

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
