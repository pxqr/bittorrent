module Network.BitTorrent.DHT.Protocol
       (
         newNodeSession

         -- * Tracker
       , ping
       , findNode
       , getPeers
       , announcePeer

         -- * Server
       , dhtServer
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Exception
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
import System.Entropy

import Remote.KRPC
import Remote.KRPC.Protocol
import Data.BEncode
import Data.Torrent.Metainfo
import Network.BitTorrent.Peer
import Network.BitTorrent.Exchange.Protocol ()

{-----------------------------------------------------------------------
    Node
-----------------------------------------------------------------------}

type NodeId = ByteString

-- WARN is the 'system' random suitable for this?
-- | Generate random NodeID used for the entire session.
--   Distribution of ID's should be as uniform as possible.
--
genNodeId :: IO NodeId
genNodeId = getEntropy 20

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
genSecret = error "secret"

-- | Instead of periodically loop over the all nodes in the routing
--   table with some given interval (or some other tricky method
--   e.g. using timeouts) we can just update tokens on demand - if no
--   one asks for a token then the token _should_ not change at all.
--
type Token = ByteString

defaultToken :: Token
defaultToken = "0xdeadbeef"

genToken :: NodeAddr -> Secret -> Token
genToken _ _ = defaultToken

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

defaultAlpha :: Alpha
defaultAlpha = 8

-- TODO
kclosest :: Int -> NodeId -> RoutingTable -> [NodeId]
kclosest = undefined

{-----------------------------------------------------------------------
    Node session
-----------------------------------------------------------------------}

data NodeSession = NodeSession {
    nodeId        :: !NodeId
  , routingTable  :: !(TVar RoutingTable)
  , contactInfo   :: !(TVar ContactInfo)
--  , currentSecret :: !(TVar Secret)
--  , secretTimestamp :: !(TVar Timestamp)
  , alpha         :: !Alpha
  , listenerPort  :: !PortNumber
  }

instance Eq NodeSession where
  (==) = (==) `on` nodeId

instance Ord NodeSession where
  compare = comparing nodeId

newNodeSession :: PortNumber -> IO NodeSession
newNodeSession lport
  = NodeSession
    <$> genNodeId
    <*> newTVarIO HM.empty
    <*> newTVarIO HM.empty
    <*> pure defaultAlpha
    <*> pure lport

assignToken :: NodeSession -> NodeId -> IO Token
assignToken _ _ = return ""

-- TODO
checkToken :: NodeId -> Token -> NodeSession -> IO Bool
checkToken _ _ _ = return True

updateTimestamp :: NodeSession -> NodeId -> IO ()
updateTimestamp = error "updateTimestamp"

updateToken :: NodeSession -> NodeId -> Token -> IO ()
updateToken _ _ _ = error "updateToken"

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

-- | Note that tracker side query functions could throw RPCException.
type DHT a b  = NodeSession -> NodeAddr -> a -> IO b

ping :: DHT () ()
ping NodeSession {..} addr @ NodeAddr {..} () = do
  nid <- call (nodeIP, nodePort) pingM nodeId
  atomically $ modifyTVar' routingTable $ HM.insert nid addr

findNode :: DHT NodeId [NodeInfo]
findNode ses @ NodeSession {..} NodeAddr {..} qnid = do
  (nid, info) <- call (nodeIP, nodePort) findNodeM (nodeId, qnid)
  updateTimestamp ses nid
  return (decodeCompact info)

getPeers :: DHT InfoHash (Either [NodeInfo] [PeerAddr])
getPeers ses @ NodeSession {..} NodeAddr {..} ih = do
    resp <- call (nodeIP, nodePort) getPeersM (nodeId, ih)
    (nid, tok, res) <- extrResp resp
    updateTimestamp ses nid
    updateToken ses nid tok
    return res
  where
    extrResp (BDict d)
      | Just (BString nid   ) <- M.lookup "id"     d
      , Just (BString tok   ) <- M.lookup "token"  d
      , Just (BList   values) <- M.lookup "values" d
      = return $ (nid, tok, Right $ decodePeerList values)

      | Just (BString nid   ) <- M.lookup "id"     d
      , Just (BString tok   ) <- M.lookup "token"  d
      , Just (BString nodes)  <- M.lookup "nodes"  d
      = return (nid, tok, Left $ decodeCompact nodes)

    extrResp  _ = throw $ RPCException msg
      where msg = ProtocolError "unable to extract getPeers resp"

-- remove token from signature, handle the all token stuff by NodeSession

-- | Note that before ever calling this method you should call the
-- getPeerList.
announcePeer :: DHT (InfoHash, Token) NodeId
announcePeer ses @ NodeSession {..} NodeAddr {..} (ih, tok) = do
  nid <- call (nodeIP, nodePort) announcePeerM (nodeId, ih, listenerPort, tok)
  updateTimestamp ses nid
  return nid

{-----------------------------------------------------------------------
    DHT Server
-----------------------------------------------------------------------}
-- TODO: update node timestamp on each successful call
-- NOTE: ensure all server operations run in O(1)

type ServerHandler a b = NodeSession -> NodeAddr -> a -> IO b

pingS :: ServerHandler NodeId NodeId
pingS NodeSession {..} addr nid = do
  atomically $ modifyTVar' routingTable $ insertNode nid addr
  return nodeId

findNodeS :: ServerHandler (NodeId, NodeId) (NodeId, CompactInfo)
findNodeS ses @ NodeSession {..} _ (nid, qnid) = do
  updateTimestamp ses nid
  rt <- atomically $ readTVar routingTable
  return (nodeId, encodeCompact $ kclosest alpha qnid rt)

getPeersS :: ServerHandler (NodeId, InfoHash) BEncode
getPeersS ses @ NodeSession {..} _ (nid, ih) = do
    updateTimestamp ses nid
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
  updateTimestamp ses nid
  registered <- checkToken nid token ses
  when registered $ do
    atomically $ do
      let peerAddr = PeerAddr Nothing nodeIP port
      modifyTVar contactInfo $ insertPeer ih peerAddr
  return nodeId

dhtTracker :: NodeSession -> InfoHash -> Chan PeerAddr -> IO ()
dhtTracker  = undefined

dhtServer :: NodeSession -> PortNumber -> IO ()
dhtServer s p = server p methods
  where
    methods =
      [ pingM         ==> pingS         s undefined
      , findNodeM     ==> findNodeS     s undefined
      , getPeersM     ==> getPeersS     s undefined
      , announcePeerM ==> announcePeerS s undefined
      ]