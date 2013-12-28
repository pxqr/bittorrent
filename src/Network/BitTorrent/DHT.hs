{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.BitTorrent.DHT
       ( dht
       , ping
       , Network.BitTorrent.DHT.bootstrap
       , Network.BitTorrent.DHT.lookup
       , Network.BitTorrent.DHT.insert
       ) where

import Control.Applicative
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


{-----------------------------------------------------------------------
--  Handlers
-----------------------------------------------------------------------}

pingH :: Address ip => NodeHandler ip
pingH = nodeHandler $ \ _ Ping -> return Ping

{-
findNodeH :: (Eq ip, Serialize ip, Typeable ip) => Handler (DHT ip)
findNodeH = dhtHandler $ \ _ (FindNode nid) ->
  NodeFound <$> getClosest nid

getPeersH :: (Eq ip, Serialize ip, Typeable ip) => Handler (DHT ip)
getPeersH = dhtHandler $ \ addr (GetPeers ih) ->
  GotPeers <$> getPeerList ih <*> grantToken addr

announceH :: Handler (DHT ip)
announceH = dhtHandler $ \ addr (Announce {..}) -> do
  checkToken addr sessionToken
  insertPeer topic undefined -- PeerAddr (add, port)
  return Announced
-}

handlers :: Address ip => [NodeHandler ip]
handlers = [pingH]

{-----------------------------------------------------------------------
--  Query
-----------------------------------------------------------------------}

-- | Run DHT on specified port. <add note about resources>
dht :: Address ip => NodeAddr ip -> DHT ip a -> IO a
dht addr = runDHT addr handlers

ping :: Address ip => NodeAddr ip -> DHT ip ()
ping addr = do
  Ping <- Ping <@> addr
  return ()

-- | One good node may be sufficient. <note about 'Data.Torrent.tNodes'>
bootstrap :: Address ip => [NodeAddr ip] -> DHT ip ()
bootstrap startNodes = do
    $(logInfoS) "bootstrap" "Start node bootstrapping"
    mapM_ insertClosest startNodes
    $(logInfoS) "bootstrap" "Node bootstrapping finished"
  where
    insertClosest addr = do
      nid <- getNodeId
      NodeFound closest <- FindNode nid <@> addr
      $(logDebug) ("Get a list of closest nodes: " <>
                  T.pack (PP.render (pretty closest)))
      forM_ closest insertNode

-- | Get list of peers which downloading
lookup :: Address ip => InfoHash -> DHT ip [PeerAddr ip]
lookup ih = getClosestHash ih >>= collect
  where
    collect nodes = L.concat <$> forM (nodeAddr <$> nodes) retrieve
    retrieve addr = do
      GotPeers {..} <- GetPeers ih <@> addr
      either collect pure peers

-- | Announce that /this/ peer may have some pieces of the specified
-- torrent.
insert :: Address ip => InfoHash -> PortNumber -> DHT ip ()
insert ih port = do
  nodes <- getClosestHash ih
  forM_ (nodeAddr <$> nodes) $ \ addr -> do
--    GotPeers {..} <- GetPeers ih <@> addr
--    Announced     <- Announce False ih undefined grantedToken <@> addr
    return ()
