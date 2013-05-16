-- TODO move to Network.DHT.Kademlia
{-# LANGUAGE OverloadedStrings #-}
module Network.DHT.Kademlia
       (
       ) where

import Data.ByteString
import Network
import Remote.KRPC



-- | Global unique identifier of the node. Size of the identifier
-- should(!) be equal to the size of DHT keys. This limitation arises
-- from the design of Kademlia: we should estimate distance between
-- keys and NodeId in routing algorithms.
--
type NodeID = ByteString

type NodeAddr = ByteString
type InfoHash = ByteString
type Token    = ByteString

-- | Used to check out if a node is alive or not. This has a tow-fold effect:
--
--     * If(!) caller get response it should update the bucket
--     corresponding to the callee.
--
--     * Callee of the ping should update the bucket corresponding
--     to the caller as well.
--
ping :: Method NodeID NodeID
ping = method "ping" ["id"] ["id"]

type PeerContact = ()
data NodeContact = NodeContact {
    peerContact :: PeerContact
  , nodeID :: NodeID
  }

-- | Used to lookup peer ID from node ID.
--
find_node :: Method (NodeID, NodeID) (NodeID, NodeContact)
find_node = method "find_node" ["id", "target"] ["id", "nodes"]

-- |
announce_peer :: Method (NodeID, InfoHash, PortNumber, Token) NodeID
announce_peer = undefined

genNodeID :: IO NodeID
genNodeID = undefined

{-
type InfoHash = Int
type Token = Int

ping :: Method NodeId NodeId
ping = method "ping" ["id"] ["id"]

get_peers :: Method (NodeId :*: InfoHash) (NodeId, Token, NodeAddr :|: NodeAddr)
get_peers = method "get_peers"
  ("id", "target")
  ("id", "token", view ("values" :|: "nodes"))




-}