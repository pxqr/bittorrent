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
       , dht
       , Network.BitTorrent.DHT.bootstrap
       , Network.BitTorrent.DHT.lookup
       , Network.BitTorrent.DHT.insert
       , Network.BitTorrent.DHT.delete
       ) where

import Control.Applicative
import Control.Exception.Lifted
import Control.Monad as M
import Control.Monad.Logger
import Control.Monad.Trans
import Data.Conduit as C
import Data.Conduit.List as C
import Data.List as L
import Data.Monoid
import Data.Text as T
import Network.Socket (PortNumber)
import Text.PrettyPrint as PP hiding ((<>), ($$))
import Text.PrettyPrint.Class

import Data.Torrent.InfoHash
import Network.KRPC (QueryFailure)
import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Message
import Network.BitTorrent.DHT.Routing
import Network.BitTorrent.DHT.Session


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

{-----------------------------------------------------------------------
--  DHT operations
-----------------------------------------------------------------------}

-- | Run DHT on specified port. <add note about resources>
dht :: Address ip
    => Options     -- ^ normally you need to use 'Data.Default.def';
    -> NodeAddr ip -- ^ address to bind this node;
    -> DHT ip a    -- ^ actions to run: 'bootstrap', 'lookup', etc;
    -> IO a        -- ^ result.
dht = runDHT handlers
{-# INLINE dht #-}

-- | One good node may be sufficient. The list of bootstrapping nodes
-- usually obtained from 'Data.Torrent.tNodes' field. Bootstrapping
-- process can take up to 5 minutes.
--
--   This operation is synchronous and do block, use 'async' if needed.
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

-- | Get list of peers which downloading this torrent.
--
--   This operation is incremental and do block.
--
lookup :: Address ip => InfoHash -> DHT ip `Source` [PeerAddr ip]
lookup topic = do      -- TODO retry getClosest if bucket is empty
  closest <- lift $ getClosest topic
  sourceList [closest] $= search topic (getPeersQ topic)

-- | Announce that /this/ peer may have some pieces of the specified
-- torrent.
--
--   This operation is synchronous and do block, use 'async' if needed.
--
insert :: Address ip => InfoHash -> PortNumber -> DHT ip ()
insert ih port = do
  nodes <- getClosest ih
  forM_ (nodeAddr <$> nodes) $ \ addr -> do
--    GotPeers {..} <- GetPeers ih <@> addr
--    Announced     <- Announce False ih undefined grantedToken <@> addr
    return ()

-- | Stop announcing /this/ peer for the specified torrent.
--
--   This operation is atomic and may block for a while.
--
delete :: Address ip => InfoHash -> DHT ip ()
delete = error "DHT.delete: not implemented"