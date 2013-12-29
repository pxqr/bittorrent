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
import Control.Concurrent.Lifted
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Logger
import Data.List as L
import Data.Monoid
import Data.Text as T
import Network.Socket (PortNumber)
import System.Timeout.Lifted
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
    Nothing    -> undefined
    Just paddr -> do
      insertPeer topic paddr
      return Announced

handlers :: Address ip => [NodeHandler ip]
handlers = [pingH, findNodeH, getPeersH, announceH]

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

-- TODO fork?
-- | One good node may be sufficient. <note about 'Data.Torrent.tNodes'>
bootstrap :: Address ip => [NodeAddr ip] -> DHT ip ()
bootstrap startNodes = do
    $(logInfoS) "bootstrap" "Start node bootstrapping"
    mapM_ insertClosest startNodes
    $(logInfoS) "bootstrap" "Node bootstrapping finished"
  where
    insertClosest addr = do
      nid <- getNodeId
      result <- try $ timeout 1000000 $ FindNode nid <@> addr
      case result of
        Left                    e -> do
          $(logWarnS) "bootstrap" $ T.pack $ show (e :: IOError)

        Right Nothing -> do
          $(logWarnS) "bootstrap" $ "not responding @ "
                                 <> T.pack (show (pretty  addr))

        Right (Just (NodeFound closest)) -> do
          $(logDebug) ("Get a list of closest nodes: " <>
                       T.pack (PP.render (pretty closest)))
          forM_ (L.take 2 closest) $ \ info @ NodeInfo {..} -> do
            insertNode    info
            let prettyAddr = T.pack (show (pretty nodeAddr))
            $(logInfoS) "bootstrap" $ "table detalization" <> prettyAddr
            fork $ insertClosest nodeAddr

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
