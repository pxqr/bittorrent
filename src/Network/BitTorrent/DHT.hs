{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.DHT
       (
       ) where

import Data.ByteString
import Network

import Data.Kademlia.Routing.Table

--type DHT k v = StateT (Table k v) IO

--findNode :: NodeID -> DHT k v [NodeInfo]
--findNode = undefined