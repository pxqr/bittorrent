{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Kademlia.Common
       (NodeID, NodeInfo
       ) where

import Control.Applicative
import Data.ByteString
import Network
import Network.Socket
import Data.Serialize


type NodeID = ByteString
type Distance = NodeID

-- WARN is the 'system' random suitable for this?
-- | Generate random NodeID used for the entire session.
--   Distribution of ID's should be as uniform as possible.
--
genNodeID :: IO NodeID
genNodeID = undefined -- randomIO

instance Serialize PortNumber where
  get = fromIntegral <$> getWord16be
  put = putWord16be . fromIntegral


data NodeAddr = NodeAddr {
    nodeIP   :: HostAddress
  , nodePort :: PortNumber
  } deriving (Show, Eq)

instance Serialize NodeAddr where
  get = NodeAddr <$> getWord32be <*> get
  put NodeAddr {..} = do
    putWord32be nodeIP
    put         nodePort


data NodeInfo = NodeInfo {
    nodeID   :: NodeID
  , nodeAddr :: NodeAddr
  } deriving (Show, Eq)

instance Serialize NodeInfo where
  get = NodeInfo <$> getByteString 20 <*> get
  put NodeInfo {..} = put nodeID >> put nodeAddr
