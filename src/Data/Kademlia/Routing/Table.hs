-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Routing table used to lookup . Internally it uses not balanced tree
--
-- TODO write module synopsis
module Data.Kademlia.Routing.Table
       ( Table(nodeID)
       ) where

import Control.Applicative
import Data.List as L
import Data.Maybe

import Data.Kademlia.Routing.Tree


data Table k v = Table {
    routeTree     :: Tree k v

    -- | Set degree of parallelism in node lookup calls.
  , alpha         :: Int
  , nodeID        :: k
  }

--insert :: NodeID -> Table -> Table
--insert x t = undefined

--closest :: InfoHash -> Table -> [NodeID]
--closest = undefined


-- TODO table serialization: usually we need to save table between
-- target program executions for bootstrapping
