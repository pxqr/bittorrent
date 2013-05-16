module Data.Kademlia.RoutingTable
       (
       ) where

import Data.ByteString

type NodeID = ByteString
type InfoHash = ByteString

type Bucket = [NodeID]

data Tree
  = Tip Bucket
  | Bin Table Table

data Table = Table {
    tree :: Tree
  , bucketSize :: Int
  }

closest :: InfoHash -> Table -> [NodeID]
closest = undefined

insert :: NodeID -> Table -> Table
insert x t = undefined

-- TODO table serialization: usually we need to save table between
-- target program executions
