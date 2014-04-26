-- |
--   Copyright   :  (c) Sam Truzjan 2014
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides functions to interact with other nodes.
--   Normally, you don't need to import this module, use
--   "Network.BitTorrent.DHT" instead.
--
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.BitTorrent.DHT.Query
       ( -- * Handler
         -- | To bind specific set of handlers you need to pass
         -- handler list to the 'startNode' function.
         pingH
       , findNodeH
       , getPeersH
       , announceH
       , defaultHandlers

         -- * Query
         -- ** Basic
         -- | A basic query perform a single request expecting a
         -- single response.
       , Iteration
       , pingQ
       , findNodeQ
       , getPeersQ
       , announceQ

         -- ** Iterative
         -- | An iterative query perform multiple basic queries,
         -- concatenate its responses, optionally yielding result and
         -- continue to the next iteration.
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
import Data.Torrent
import Network.BitTorrent.Address
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
      insertNode (NodeInfo remoteId naddr) -- TODO need to block. why?
      Response <$> asks thisNodeId <*> action naddr q

-- | Default 'Ping' handler.
pingH :: Address ip => NodeHandler ip
pingH = nodeHandler $ \ _ Ping -> do
  return Ping

-- | Default 'FindNode' handler.
findNodeH :: Address ip => NodeHandler ip
findNodeH = nodeHandler $ \ _ (FindNode nid) -> do
  NodeFound <$> getClosest nid

-- | Default 'GetPeers' handler.
getPeersH :: Address ip => NodeHandler ip
getPeersH = nodeHandler $ \ naddr (GetPeers ih) -> do
  GotPeers <$> getPeerList ih <*> grantToken naddr

-- | Default 'Announce' handler.
announceH :: Address ip => NodeHandler ip
announceH = nodeHandler $ \ naddr @ NodeAddr {..} (Announce {..}) -> do
  valid <- checkToken naddr sessionToken
  unless valid $ do
    throwIO $ InvalidParameter "token"

  let annPort  = if impliedPort then nodePort else port
  let peerAddr = PeerAddr Nothing nodeHost annPort
  insertPeer topic peerAddr
  return Announced

-- | Includes all default query handlers.
defaultHandlers :: Address ip => [NodeHandler ip]
defaultHandlers = [pingH, findNodeH, getPeersH, announceH]

{-----------------------------------------------------------------------
--  Basic queries
-----------------------------------------------------------------------}

type Iteration ip o = NodeInfo ip -> DHT ip (Either [NodeInfo ip] [o ip])

-- | The most basic query. May be used to check if the given node is
-- alive or get its 'NodeId'.
pingQ :: Address ip => NodeAddr ip -> DHT ip (NodeInfo ip)
pingQ addr = do
  (nid, Ping) <- queryNode addr Ping
  return (NodeInfo nid addr)

-- TODO [robustness] match range of returned node ids with the
-- expected range and either filter bad nodes or discard response at
-- all throwing an exception
findNodeQ :: Address ip => TableKey key => key -> Iteration ip NodeInfo
findNodeQ key NodeInfo {..} = do
  NodeFound closest <- FindNode (toNodeId key) <@> nodeAddr
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

{-----------------------------------------------------------------------
--  Iterative queries
-----------------------------------------------------------------------}

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
  i <- asks (optReannounce . options)
  error "DHT.republish: not implemented"
