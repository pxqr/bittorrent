-- | For more info see:
--   <http://www.bittorrent.org/beps/bep_0005.html#dht-queries>
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Network.BitTorrent.DHT.Message
       ( -- * Envelopes
         Query (..)
       , Response (..)

         -- * Queries
         -- ** ping
       , Ping (..)

         -- ** find_node
       , FindNode (..)
       , NodeFound (..)

         -- ** get_peers
       , GetPeers (..)
       , GotPeers (..)

         -- ** announce_peer
       , Announce  (..)
       , Announced (..)
       ) where

import Control.Applicative
import Data.BEncode as BE
import Data.BEncode.BDict
import Data.ByteString as BS
import Data.Monoid
import Data.Serialize as S
import Data.Typeable
import Network
import Network.KRPC

import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.KRPC ()

{-----------------------------------------------------------------------
-- envelopes
-----------------------------------------------------------------------}

type Token = ByteString

node_id_key :: BKey
node_id_key = "id"

-- | All queries have an "id" key and value containing the node ID of
-- the querying node.
data Query a = Query
  { thisNodeId  :: NodeId
  , queryParams :: a
  } deriving (Show, Eq)

instance BEncode a => BEncode (Query a) where
  toBEncode Query {..} = toDict $
      node_id_key .=! thisNodeId .: endDict
      <>
      dict (toBEncode queryParams)
    where
      dict (BDict d) = d
      dict    _      = error "impossible: instance BEncode (Query a)"

  fromBEncode v = do
    Query <$> fromDict (field (req node_id_key)) v
          <*> fromBEncode v

-- | All responses have an "id" key and value containing the node ID
-- of the responding node.
data Response a = Response
  { remoteNodeId :: NodeId
  , responseVals :: a
  } deriving (Show, Eq)

instance BEncode a => BEncode (Response a) where
  toBEncode = toBEncode . toQuery
    where
      toQuery (Response nid a) = Query nid a

  fromBEncode b = fromQuery <$> fromBEncode b
    where
      fromQuery (Query nid a) = Response nid a


{-----------------------------------------------------------------------
-- ping method
-----------------------------------------------------------------------}

-- | The most basic query is a ping. Ping query is used to check if a
-- quered node is still alive.
data Ping = Ping
  deriving (Show, Eq, Typeable)

instance BEncode Ping where
  toBEncode Ping = toDict endDict
  fromBEncode _  = pure Ping

-- | \"q\" = \"ping\"
instance KRPC (Query Ping) (Response Ping) where
  method = "ping"

{-----------------------------------------------------------------------
-- find_node method
-----------------------------------------------------------------------}

-- | Find node is used to find the contact information for a node
-- given its ID.
newtype FindNode = FindNode NodeId
  deriving (Show, Eq, Typeable)

target_key :: BKey
target_key = "target"

instance BEncode FindNode where
  toBEncode (FindNode nid) = toDict   $ target_key .=! nid .: endDict
  fromBEncode              = fromDict $ FindNode  <$>! target_key

-- | When a node receives a 'FindNode' query, it should respond with a
-- the compact node info for the target node or the K (8) closest good
-- nodes in its own routing table.
--
newtype NodeFound ip = NodeFound [NodeInfo ip]
  deriving (Show, Eq, Typeable)

nodes_key :: BKey
nodes_key = "nodes"

binary :: Serialize a => BKey -> BE.Get [a]
binary k = field (req k) >>= either (fail . format) return .
    runGet (many get)
  where
    format str = "fail to deserialize " ++ show k ++ " field: " ++ str

instance (Typeable ip, Serialize ip) => BEncode (NodeFound ip) where
  toBEncode (NodeFound ns) = toDict $
       nodes_key .=! S.encode ns
    .: endDict

  fromBEncode = fromDict $ NodeFound <$> binary nodes_key

-- | \"q\" == \"find_node\"
instance (Serialize ip, Typeable ip)
      => KRPC (Query FindNode) (Response (NodeFound ip)) where
  method = "find_node"

{-----------------------------------------------------------------------
-- get_peers method
-----------------------------------------------------------------------}

-- | Get peers associated with a torrent infohash.
newtype GetPeers = GetPeers InfoHash
  deriving Typeable

info_hash_key :: BKey
info_hash_key = "info_hash"

instance BEncode GetPeers where
  toBEncode (GetPeers ih) = toDict   $ info_hash_key .=! ih .: endDict
  fromBEncode             = fromDict $ GetPeers <$>! info_hash_key

data GotPeers ip = GotPeers
  { -- | If the queried node has no peers for the infohash, returned
    -- the K nodes in the queried nodes routing table closest to the
    -- infohash supplied in the query.
    peers        :: Either [NodeInfo ip] [PeerAddr ip]

    -- | The token value is a required argument for a future
    -- announce_peer query.
  , grantedToken :: Token
  } deriving Typeable

peers_key :: BKey
peers_key = "values"

token_key :: BKey
token_key = "token"

instance (Typeable ip, Serialize ip) => BEncode (GotPeers ip) where
  toBEncode GotPeers {..} = toDict $
       putPeerList peers
    .: token_key .=! grantedToken
    .: endDict
    where
      putPeerList (Right ps) = peers_key .=! S.encode ps
      putPeerList (Left  ns) = nodes_key .=! S.encode ns

  fromBEncode = fromDict $ GotPeers <$> getPeerList  <*>! token_key
    where
      getPeerList = Right <$> binary peers_key
                <|> Left  <$> binary nodes_key

instance (Typeable ip, Serialize ip) =>
         KRPC (Query GetPeers) (Response (GotPeers ip)) where
  method = "get_peers"

{-----------------------------------------------------------------------
-- announce method
-----------------------------------------------------------------------}

-- | Announce that the peer, controlling the querying node, is
-- downloading a torrent on a port.
data Announce = Announce
  { -- | infohash of the torrent;
    topic    :: InfoHash

    -- | the port /this/ peer is listening;
  , port     :: PortNumber

    -- | received in response to a previous get_peers query.
  , sessionToken :: Token
  } deriving Typeable

port_key :: BKey
port_key = "port"

instance BEncode Announce where
  toBEncode Announce {..} = toDict $
       info_hash_key .=! topic
    .: port_key      .=! port
    .: token_key     .=! sessionToken
    .: endDict
  fromBEncode = fromDict $ do
    Announce <$>! info_hash_key
             <*>! port_key
             <*>! token_key

-- | The queried node must verify that the token was previously sent
-- to the same IP address as the querying node. Then the queried node
-- should store the IP address of the querying node and the supplied
-- port number under the infohash in its store of peer contact
-- information.
data Announced = Announced

instance BEncode Announced where
  toBEncode   _ = toBEncode Ping
  fromBEncode _ = pure  Announced

instance KRPC (Query Announce) (Response Announced) where
  method = "announce_peer"