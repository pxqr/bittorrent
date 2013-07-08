{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.BitTorrent.DHT
       ( dhtServer
       ) where

import Control.Applicative
import Control.Concurrent.STM
import Data.ByteString
import Data.Serialize as S
import Data.Function
import Data.Ord
import Data.HashMap.Strict as HM

import Network
import Network.Socket
import Remote.KRPC

import Data.BEncode
import Data.Torrent
import Data.Kademlia.Routing.Table

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
    nodeIP   :: !HostAddress
  , nodePort :: !PortNumber
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

decodeCompact :: CompactInfo -> Either String [NodeInfo]
decodeCompact = S.runGet (many get)

encodeCompact :: [NodeId] -> CompactInfo
encodeCompact = S.runPut . mapM_ put

type Distance = NodeId

--type DHT k v = StateT (Table k v) IO

--findNode :: NodeID -> DHT k v [NodeInfo]
--findNode = undefined

genSecret :: IO Secret
genSecret = undefined

type Token = Int
type Secret = Int

token :: NodeAddr -> Secret -> Token
token = return undefined

defaultToken :: Token
defaultToken = 0xdeadbeef

{-----------------------------------------------------------------------
    Routing table
-----------------------------------------------------------------------}

-- TODO use more compact routing table
type RoutingTable = HashMap NodeId NodeAddr

type Alpha = Int

kclosest :: Int -> NodeId -> RoutingTable -> [NodeId]
kclosest = undefined

{-----------------------------------------------------------------------
    Node session
-----------------------------------------------------------------------}

data NodeSession = NodeSession {
    nodeId       :: !NodeId
  , routingTable :: !(TVar RoutingTable)
  , alpha        :: !Alpha
  }

instance Eq NodeSession where
  (==) = (==) `on` nodeId

instance Ord NodeSession where
  compare = comparing nodeId

{-----------------------------------------------------------------------
    Queries
-----------------------------------------------------------------------}

instance BEncodable PortNumber where

pingM :: Method NodeId NodeId
pingM = method "ping" ["id"] ["id"]

findNodeM :: Method (NodeId, NodeId) (NodeId, CompactInfo)
findNodeM = method "find_node" ["id", "target"] ["id", "nodes"]

-- | Lookup peers by a torrent infohash.
getPeersM :: Method (NodeId, InfoHash) (NodeId, Token, CompactInfo) -- use Map ByteString BEncode
getPeersM = method "get_peers" ["id", "info_hash"] ["id", "token", "nodes"]

-- | Used to announce that the peer, controlling the quering node is
-- downloading a torrent on a port.
announcePeerM :: Method (NodeId, InfoHash, PortNumber, Token) NodeId
announcePeerM = method "announce_peer" ["id", "info_hash", "port", "token"] ["id"]

pingC :: NodeSession -> NodeAddr -> IO ()
pingC NodeSession {..} addr @ NodeAddr {..}  = do
  nid <- call (nodeIP, nodePort) pingM nodeId
  atomically $ modifyTVar' routingTable $ HM.insert nid addr

--getPeerC :: NodeSession -> NodeAddr -> InfoHash -> IO (Either CompactInfo )
getPeerC NodeSession {..} addr @ NodeAddr {..} ih = do
  call (nodeIP, nodePort) getPeersM

type ServerHandler a b = NodeSession -> NodeAddr -> a -> IO b

pingS :: ServerHandler NodeId NodeId
pingS NodeSession {..} addr nid = do
  atomically $ modifyTVar' routingTable $ HM.insert nid addr
  return nodeId

findNodeS :: ServerHandler (NodeId, NodeId) (NodeId, CompactInfo)
findNodeS NodeSession {..} addr (nid, qnid) = do
  rt <- atomically $ readTVar routingTable
  return (nodeId, encodeCompact $ kclosest alpha qnid rt)

getPeersS :: ServerHandler (NodeId, InfoHash) (NodeId, Token, CompactInfo)
getPeersS NodeSession {..} addr (nid, ih) = do

  return (nodeId, defaultToken, error "compact info")

announcePeerS :: ServerHandler (NodeId, InfoHash, PortNumber, Token) NodeId
announcePeerS NodeSession {..} addr (nid, ih, port, token) = do
  let right = (error "checkToken")
  return nodeId

dhtServer :: PortNumber -> NodeSession -> IO ()
dhtServer p s = server p
  [ pingM         ==> pingS         s undefined
  , findNodeM     ==> findNodeS     s undefined
  , getPeersM     ==> getPeersS     s undefined
  , announcePeerM ==> announcePeerS s undefined
  ]