{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.BitTorrent.DHT
       (
         -- * Tracker
         ping
       , findNode
       , getPeers
       , announcePeer

         -- * Server
       , dhtServer
       ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString
import Data.Serialize as S
import Data.Function
import Data.Ord
import Data.Maybe
import Data.List as L
import Data.Map as M
import Data.HashMap.Strict as HM

import Network
import Network.Socket
import Remote.KRPC

import Data.BEncode
import Data.Torrent
import Data.Kademlia.Routing.Table
import Network.BitTorrent.Peer

{-----------------------------------------------------------------------
    Node
-----------------------------------------------------------------------}

type NodeId = ByteString

-- WARN is the 'system' random suitable for this?
-- | Generate random NodeID used for the entire session.
--   Distribution of ID's should be as uniform as possible.
--
genNodeID :: IO NodeId
genNodeID = undefined -- randomIO

instance Serialize PortNumber where
  get = fromIntegral <$> getWord16be
  put = putWord16be . fromIntegral


data NodeAddr = NodeAddr {
    nodeIP   :: {-# UNPACK #-} !HostAddress
  , nodePort :: {-# UNPACK #-} !PortNumber
  } deriving (Show, Eq)

instance Serialize NodeAddr where
  get = NodeAddr <$> getWord32be <*> get
  put NodeAddr {..} = do
    putWord32be nodeIP
    put         nodePort


data NodeInfo = NodeInfo {
    nodeID   :: !NodeId
  , nodeAddr :: !NodeAddr
  } deriving (Show, Eq)

instance Serialize NodeInfo where
  get = NodeInfo <$> getByteString 20 <*> get
  put NodeInfo {..} = put nodeID >> put nodeAddr

type CompactInfo = ByteString

decodeCompact :: CompactInfo -> [NodeInfo]
decodeCompact = either (const []) id . S.runGet (many get)

encodeCompact :: [NodeId] -> CompactInfo
encodeCompact = S.runPut . mapM_ put

decodePeerList :: [BEncode] -> [PeerAddr]
decodePeerList = undefined

encodePeerList :: [PeerAddr] -> [BEncode]
encodePeerList = undefined

type Distance = NodeId


{-----------------------------------------------------------------------
    Tokens
-----------------------------------------------------------------------}

type Secret = Int

genSecret :: IO Secret
genSecret = undefined

-- | Instead of periodically loop over the all nodes in the routing
--   table with some given interval (or some other tricky method
--   e.g. using timeouts) we can just update tokens on demand - if no
--   one asks for a token then the token _should_ not change at all.
--
type Token = ByteString

genToken :: NodeAddr -> Secret -> Token
genToken = return undefined

defaultToken :: Token
defaultToken = "0xdeadbeef"

{-----------------------------------------------------------------------
    Routing table
-----------------------------------------------------------------------}

type ContactInfo  = HashMap InfoHash [PeerAddr]

insertPeer :: InfoHash -> PeerAddr -> ContactInfo -> ContactInfo
insertPeer ih addr = HM.insertWith (++) ih [addr]

lookupPeers :: InfoHash -> ContactInfo -> [PeerAddr]
lookupPeers ih = fromMaybe [] . HM.lookup ih

-- TODO use more compact routing table
type RoutingTable = HashMap NodeId NodeAddr

insertNode :: NodeId -> NodeAddr -> RoutingTable -> RoutingTable
insertNode = HM.insert

type Alpha = Int

-- TODO
kclosest :: Int -> NodeId -> RoutingTable -> [NodeId]
kclosest = undefined

{-----------------------------------------------------------------------
    Node session
-----------------------------------------------------------------------}

data NodeSession = NodeSession {
    nodeId       :: !NodeId
  , routingTable :: !(TVar RoutingTable)
  , contactInfo  :: !(TVar ContactInfo)
  , alpha        :: !Alpha
  , listenerPort :: !PortNumber
  }

instance Eq NodeSession where
  (==) = (==) `on` nodeId

instance Ord NodeSession where
  compare = comparing nodeId

assignToken :: NodeSession -> NodeId -> IO Token
assignToken _ _ = return ""

-- TODO
checkToken :: NodeId -> Token -> NodeSession -> IO Bool
checkToken nid token _ = return True

{-----------------------------------------------------------------------
    DHT Queries
-----------------------------------------------------------------------}

pingM :: Method NodeId NodeId
pingM = method "ping" ["id"] ["id"]

findNodeM :: Method (NodeId, NodeId) (NodeId, CompactInfo)
findNodeM = method "find_node" ["id", "target"] ["id", "nodes"]

-- | Lookup peers by a torrent infohash. This method might return
-- different kind of responses depending on the routing table of
-- queried node:
--
--   * If quieried node contains a peer list for the given infohash
--   then the node should return the list in a "value" key. Note that
--   list is encoded as compact peer address, not a compact node info.
--   The result of 'get_peers' method have the following scheme:
--
--      > { "id"     : "dht_server_node_id"
--      > , "token"  : "assigned_token"
--      > , "values" : ["_IP_PO", "_ip_po"]
--      > }
--
--   * If quieried node does not contain a list of peers associated
--   with the given infohash, then node should return
--
--      > { "id"    : "dht_server_node_id"
--      > , "token" : "assigned_token"
--      > , "nodes" : "compact_nodes_info"
--      > }
--
--   The resulting dictionaries might differ only in a values\/nodes
--   keys.
--
getPeersM :: Method (NodeId, InfoHash) BEncode
getPeersM = method "get_peers" ["id", "info_hash"] []

-- | Used to announce that the peer, controlling the quering node is
-- downloading a torrent on a port.
announcePeerM :: Method (NodeId, InfoHash, PortNumber, Token) NodeId
announcePeerM = method "announce_peer" ["id", "info_hash", "port", "token"] ["id"]

{-----------------------------------------------------------------------
    DHT Tracker
-----------------------------------------------------------------------}
-- TODO: update node timestamp on each successful call

type DHT a b  = NodeSession -> NodeAddr -> a -> IO b

ping :: DHT () ()
ping NodeSession {..} addr @ NodeAddr {..} () = do
  nid <- call (nodeIP, nodePort) pingM nodeId
  atomically $ modifyTVar' routingTable $ HM.insert nid addr

findNode :: DHT NodeId [NodeInfo]
findNode NodeSession {..} NodeAddr {..} qnid = do
  (_, info) <- call (nodeIP, nodePort) findNodeM (nodeId, qnid)
  return (decodeCompact info)

getPeers :: DHT InfoHash (Either [NodeInfo] [PeerAddr])
getPeers NodeSession {..} NodeAddr {..} ih = do
    extrResp <$> call (nodeIP, nodePort) getPeersM (nodeId, ih)
  where
    extrResp (BDict d)
      | Just (BList   values) <- M.lookup "values" d
      = Right $ decodePeerList values
      | Just (BString nodes)  <- M.lookup "nodes"  d
      = Left  $ decodeCompact  nodes
    extrResp  _ = return undefined

-- remove token from signature, handle the all token stuff by NodeSession

-- | Note that before ever calling this method you should call the
-- getPeerList.
announcePeer :: DHT (InfoHash, Token) NodeId
announcePeer NodeSession {..} NodeAddr {..} (ih, tok) = do
  call (nodeIP, nodePort) announcePeerM (nodeId, ih, listenerPort, tok)

{-----------------------------------------------------------------------
    DHT Server
-----------------------------------------------------------------------}
-- TODO: update node timestamp on each successful call
-- NOTE: ensure all server operations should run in O(1)

type ServerHandler a b = NodeSession -> NodeAddr -> a -> IO b

pingS :: ServerHandler NodeId NodeId
pingS NodeSession {..} addr nid = do
  atomically $ modifyTVar' routingTable $ insertNode nid addr
  return nodeId

findNodeS :: ServerHandler (NodeId, NodeId) (NodeId, CompactInfo)
findNodeS NodeSession {..} _ (_, qnid) = do
  rt <- atomically $ readTVar routingTable
  return (nodeId, encodeCompact $ kclosest alpha qnid rt)

getPeersS :: ServerHandler (NodeId, InfoHash) BEncode
getPeersS ses @ NodeSession {..} _ (nid, ih) = do
    mkResp <$> assignToken ses nid <*> findPeers
  where
    findPeers = do
      list <- lookupPeers ih <$> readTVarIO contactInfo
      if not (L.null list)
        then return $ Right list
        else do
          rt    <- readTVarIO routingTable
          let nodes = kclosest alpha (getInfoHash ih) rt
          return $ Left nodes

    mkDict tok res = [("id",BString nodeId), ("token", BString tok), res]
    mkResult (Left  nodes ) = ("nodes",  BString $ encodeCompact  nodes)
    mkResult (Right values) = ("values", BList   $ encodePeerList values)
    mkResp tok = BDict . M.fromList . mkDict tok . mkResult

announcePeerS :: ServerHandler (NodeId, InfoHash, PortNumber, Token) NodeId
announcePeerS ses @ NodeSession {..} NodeAddr {..} (nid, ih, port, token) = do
  registered <- checkToken nid token ses
  when registered $ do
    atomically $ do
      let peerAddr = PeerAddr Nothing nodeIP port
      modifyTVar contactInfo $ insertPeer ih peerAddr
  return nodeId

dhtServer :: PortNumber -> NodeSession -> IO ()
dhtServer p s = server p
  [ pingM         ==> pingS         s undefined
  , findNodeM     ==> findNodeS     s undefined
  , getPeersM     ==> getPeersS     s undefined
  , announcePeerM ==> announcePeerS s undefined
  ]