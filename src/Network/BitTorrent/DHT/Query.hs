{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.BitTorrent.DHT.Query
       ( -- * Handler
         pingH
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

import Control.Applicative
import Control.Concurrent.Lifted hiding (yield)
import Control.Exception.Lifted hiding (Handler)
import Control.Monad.Reader
import Control.Monad.Logger
import Data.Conduit
import Data.Conduit.List as C hiding (mapMaybe, mapM_)
import Data.Either
import Data.List as L
import Data.Monoid
import Data.Text as T
import Network
import Text.PrettyPrint as PP hiding ((<>), ($$))
import Text.PrettyPrint.Class

import Network.KRPC hiding (Options, def)
import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Message
import Network.BitTorrent.DHT.Routing
import Network.BitTorrent.DHT.Session

{-----------------------------------------------------------------------
--  Handlers
-----------------------------------------------------------------------}

nodeHandler :: Address ip => KRPC (Query a) (Response b)
           => (NodeAddr ip -> a -> DHT ip b) -> NodeHandler ip
nodeHandler action = handler $ \ sockAddr (Query remoteId q) -> do
  case fromSockAddr sockAddr of
    Nothing    -> throwIO BadAddress
    Just naddr -> do
      insertNode (NodeInfo remoteId naddr)
      Response <$> getNodeId <*> action naddr q

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
