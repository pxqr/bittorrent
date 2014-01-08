-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides message datatypes which is used for /Node to
--   Node/ communication. Bittorrent DHT is based on Kademlia
--   specification, but have a slightly different set of messages
--   which have been adopted for /peer/ discovery mechanism. Messages
--   are sent over "Network.KRPC" protocol, but normally you should
--   use "Network.BitTorrent.DHT.Session" to send and receive
--   messages.
--
--   DHT queries are not /recursive/, they are /iterative/. This means
--   that /querying/ node . While original specification (namely BEP5)
--   do not impose any restrictions for /quered/ node behaviour, a
--   good DHT implementation should follow some rules to guarantee
--   that unlimit recursion will never happen. The following set of
--   restrictions:
--
--     * 'Ping' query must not trigger any message.
--
--     * 'FindNode' query /may/ trigger 'Ping' query to check if a
--     list of nodes to return is /good/. See
--     'Network.BitTorrent.DHT.Routing.Routing' for further explanation.
--
--     * 'GetPeers' query may trigger 'Ping' query for the same reason.
--
--     * 'Announce' query must trigger 'Ping' query for the same reason.
--
--   It is easy to see that the most long RPC chain is:
--
--   @
--     |                                            |                |
--   Node_A                                         |                |
--     |   FindNode or GetPeers or Announce         |                |
--     |  ------------------------------------>   Node_B             |
--     |                                            |      Ping      |
--     |                                            |  ----------->  |
--     |                                            |              Node_C
--     |                                            |      Pong      |
--     |     NodeFound or GotPeers or Announced     |  <-----------  |
--     |  <-------------------------------------  Node_B             |
--   Node_A                                         |                |
--     |                                            |                |
--   @
--
--   where in some cases 'Node_C' is 'Node_A'.
--
--   For more info see:
--   <http://www.bittorrent.org/beps/bep_0005.html#dht-queries>
--
--   For Kamelia messages see original Kademlia paper:
--   <http://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf>
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
import Data.List as L
import Data.Monoid
import Data.Serialize as S
import Data.Typeable
import Network
import Network.KRPC

import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Token
import Network.KRPC ()

{-----------------------------------------------------------------------
-- envelopes
-----------------------------------------------------------------------}

node_id_key :: BKey
node_id_key = "id"

-- | All queries have an \"id\" key and value containing the node ID
-- of the querying node.
data Query a = Query
  { queringNodeId :: NodeId -- ^ node id of /quering/ node;
  , queryParams   :: a      -- ^ query parameters.
  } deriving (Show, Eq, Typeable)

instance BEncode a => BEncode (Query a) where
  toBEncode Query {..} = toDict $
      node_id_key .=! queringNodeId .: endDict
      <>
      dict (toBEncode queryParams)
    where
      dict (BDict d) = d
      dict    _      = error "impossible: instance BEncode (Query a)"

  fromBEncode v = do
    Query <$> fromDict (field (req node_id_key)) v
          <*> fromBEncode v

-- | All responses have an \"id\" key and value containing the node ID
-- of the responding node.
data Response a = Response
  { queredNodeId :: NodeId -- ^ node id of /quered/ node;
  , responseVals :: a      -- ^ query result.
  } deriving (Show, Eq, Typeable)

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
       nodes_key .=! runPut (mapM_ put ns)
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
  deriving (Show, Eq, Typeable)

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
  } deriving (Show, Eq, Typeable)

peers_key :: BKey
peers_key = "values"

token_key :: BKey
token_key = "token"

instance (Typeable ip, Serialize ip) => BEncode (GotPeers ip) where
  toBEncode GotPeers {..} = toDict $
    case peers of
      Left  ns ->
           nodes_key .=! runPut (mapM_ put ns)
        .: token_key .=! grantedToken
        .: endDict
      Right ps ->
           token_key .=! grantedToken
        .: peers_key .=! L.map S.encode ps
        .: endDict

  fromBEncode = fromDict $ do
     mns <- optional (binary nodes_key)                      -- "nodes"
     tok <- field    (req    token_key)                      -- "token"
     mps <- optional (field (req peers_key) >>= decodePeers) -- "values"
     case (Right <$> mps) <|> (Left <$> mns) of
       Nothing -> fail "get_peers: neihter peers nor nodes key is valid"
       Just xs -> pure $ GotPeers xs tok
     where
       decodePeers = either fail pure . mapM S.decode

-- | \"q" = \"get_peers\"
instance (Typeable ip, Serialize ip) =>
         KRPC (Query GetPeers) (Response (GotPeers ip)) where
  method = "get_peers"

{-----------------------------------------------------------------------
-- announce method
-----------------------------------------------------------------------}

-- | Announce that the peer, controlling the querying node, is
-- downloading a torrent on a port.
data Announce = Announce
  { -- | If set, the 'port' field should be ignored and the source
    -- port of the UDP packet should be used as the peer's port
    -- instead. This is useful for peers behind a NAT that may not
    -- know their external port, and supporting uTP, they accept
    -- incoming connections on the same port as the DHT port.
    impliedPort :: Bool

    -- | infohash of the torrent;
  , topic    :: InfoHash

    -- | the port /this/ peer is listening;
  , port     :: PortNumber

    -- | received in response to a previous get_peers query.
  , sessionToken :: Token
  } deriving (Show, Eq, Typeable)

port_key :: BKey
port_key = "port"

implied_port_key :: BKey
implied_port_key = "implied_port"

instance BEncode Announce where
  toBEncode Announce {..} = toDict $
       implied_port_key .=? flagField impliedPort
    .: info_hash_key    .=! topic
    .: port_key         .=! port
    .: token_key        .=! sessionToken
    .: endDict
    where
      flagField flag = if flag then Just (1 :: Int) else Nothing

  fromBEncode = fromDict $ do
    Announce <$> (boolField <$> optional (field (req implied_port_key)))
             <*>! info_hash_key
             <*>! port_key
             <*>! token_key
    where
      boolField = maybe False (/= (0 :: Int))

-- | The queried node must verify that the token was previously sent
-- to the same IP address as the querying node. Then the queried node
-- should store the IP address of the querying node and the supplied
-- port number under the infohash in its store of peer contact
-- information.
data Announced = Announced
  deriving (Show, Eq, Typeable)

instance BEncode Announced where
  toBEncode   _ = toBEncode Ping
  fromBEncode _ = pure  Announced

-- | \"q" = \"announce\"
instance KRPC (Query Announce) (Response Announced) where
  method = "announce_peer"
