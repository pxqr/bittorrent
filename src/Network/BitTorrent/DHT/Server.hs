module Network.BitTorrent.DHT.Server
       (
       ) where

import Control.Monad.Trans.State
import Data.Kademlia.Routing.Table
import Data.Kademlia.Common



type DHT k v = StateT (Table NodeInfo InfoHash) IO

ping :: NodeID -> DHT k v NodeID
ping nid = do
--  update nid
--  gets nodeID
  undefined

findNode :: NodeID -> DHT k v [NodeInfo]
findNode = undefined

-- | Bittorrent /get_peers/ RPC is special case of the /find_value/.
findValue :: NodeID -> DHT k v (Either [NodeID] v)
findValue = undefined

-- | Bittorrent /announce_peer/ RPC is special case of the /store/.
store :: NodeID -> (k, v) -> DHT k v NodeID
store = undefined