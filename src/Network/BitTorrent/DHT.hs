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
import Control.Concurrent.Lifted hiding (yield)
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Logger
import Data.List as L
import Data.Monoid
import Data.Text as T
import Network.Socket (PortNumber)
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class

import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Message
import Network.BitTorrent.DHT.Session
import Network.KRPC


{-----------------------------------------------------------------------
--  Handlers
-----------------------------------------------------------------------}

pingH :: Address ip => NodeHandler ip
pingH = nodeHandler $ \ _ Ping -> do
  $(logDebug) "ping received, sending pong"
  return Ping

findNodeH :: Address ip => NodeHandler ip
findNodeH = nodeHandler $ \ _ (FindNode nid) -> do
  $(logDebug) "find_node received, sending closest nodes back"
  NodeFound <$> getClosest nid

getPeersH :: Address ip => NodeHandler ip
getPeersH = nodeHandler $ \ naddr (GetPeers ih) -> do
  $(logDebug) "get_peers received, trying to find peers"
  GotPeers <$> getPeerList ih <*> grantToken naddr

announceH :: Address ip => NodeHandler ip
announceH = nodeHandler $ \ naddr (Announce {..}) -> do
  $(logDebug) "announce received, trying to check token"
  checkToken naddr sessionToken
  case fromAddr naddr of
    Nothing    -> throw $ KError ProtocolError "bad address" ""
    Just paddr -> do
      insertPeer topic paddr
      return Announced

handlers :: Address ip => [NodeHandler ip]
handlers = [pingH, findNodeH, getPeersH, announceH]

{-----------------------------------------------------------------------
--  Query
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
-- (TODO) This operation is asynchronous and do not block.
--
bootstrap :: Address ip => [NodeAddr ip] -> DHT ip ()
bootstrap startNodes = do
    $(logInfoS) "bootstrap" "Start node bootstrapping"
    mapM_ insertClosest startNodes
    $(logInfoS) "bootstrap" "Node bootstrapping finished"
  where
    insertClosest addr = do
      nid <- getNodeId
      result <- try $ FindNode nid <@> addr
      case result of
        Left                    e -> do
          $(logWarnS) "bootstrap" $ T.pack $ show (e :: IOError)

        Right (NodeFound closest) -> do
          $(logDebug) $ "Get a list of closest nodes: " <>
                         T.pack (PP.render (pretty closest))
          forM_ closest $ \ info @ NodeInfo {..} -> do
            let prettyAddr = T.pack (show (pretty nodeAddr))
            $(logInfoS) "bootstrap" $ "table detalization" <> prettyAddr
            insertClosest nodeAddr

-- | Get list of peers which downloading this torrent.
--
-- (TODO) This operation is synchronous and do block.
--
lookup :: Address ip => InfoHash -> DHT ip [PeerAddr ip]
lookup topic = getClosestHash topic >>= collect
     -- TODO retry getClosestHash if bucket is empty
  where
    collect nodes = L.concat <$> forM (nodeAddr <$> nodes) retrieve
    retrieve addr = do
      GotPeers {..} <- GetPeers topic <@> addr
      either collect pure peers

-- | Announce that /this/ peer may have some pieces of the specified
-- torrent.
--
-- (TODO) This operation is asynchronous and do not block.
--
insert :: Address ip => InfoHash -> PortNumber -> DHT ip ()
insert ih port = do
  nodes <- getClosestHash ih
  forM_ (nodeAddr <$> nodes) $ \ addr -> do
--    GotPeers {..} <- GetPeers ih <@> addr
--    Announced     <- Announce False ih undefined grantedToken <@> addr
    return ()

-- | Stop announcing /this/ peer for the specified torrent.
--
--   This operation is atomic and do not block.
--
delete :: Address ip => InfoHash -> DHT ip ()
delete = error "DHT.delete: not implemented"