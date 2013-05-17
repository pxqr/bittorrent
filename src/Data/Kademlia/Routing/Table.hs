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
       ( -- ContactInfo
--       , Table
       ) where

import Control.Applicative
import Data.ByteString
import Data.List as L
import Data.Maybe

import Network.BitTorrent.Peer

{-
type NodeID = ByteString
type InfoHash = ByteString

data ContactInfo = ContactInfo {
    peerAddr :: PeerAddr
  , nodeID   :: NodeID
  } deriving (Show, Eq)


type Distance = NodeID

-- |
data Table = Table {
    routeTree     :: Tree
  , maxBucketSize :: Int
  }

insert :: NodeID -> Table -> Table
insert x t = undefined

closest :: InfoHash -> Table -> [NodeID]
closest = undefined


-- TODO table serialization: usually we need to save table between
-- target program executions for bootstrapping
-}