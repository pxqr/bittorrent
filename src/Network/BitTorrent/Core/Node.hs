-- |
--   Module      :  Network.BitTorrent.Core.Node
--   Copyright   :  (c) Sam Truzjan 2013
--                  (c) Daniel Gröber 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   A \"node\" is a client\/server listening on a UDP port
--   implementing the distributed hash table protocol. The DHT is
--   composed of nodes and stores the location of peers. BitTorrent
--   clients include a DHT node, which is used to contact other nodes
--   in the DHT to get the location of peers to download from using
--   the BitTorrent protocol.
--
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
module Network.BitTorrent.Core.Node
       (  -- * Node ID
         NodeId
       , testIdBit
       , genNodeId

         -- * Node address
       , NodeAddr (..)

         -- * Node info
       , NodeInfo (..)
       ) where

import Control.Applicative
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.TH
import Data.Bits
import Data.ByteString as BS
import Data.BEncode as BE
import Data.Default
import Data.Hashable
import Data.IP
import Data.List as L
import Data.Ord
import Data.Serialize as S
import Data.String
import Data.Typeable
import Data.Word
import Network
import System.Entropy

import Data.Torrent.JSON
import Network.BitTorrent.Core.PeerAddr (PeerAddr (..))

{-----------------------------------------------------------------------
--  Node id
-----------------------------------------------------------------------}
-- TODO more compact representation ('ShortByteString's?)

-- | Each node has a globally unique identifier known as the \"node
-- ID.\"
--
--   Normally, /this/ node id should we saved between invocations
--   of the client software.
newtype NodeId = NodeId ByteString
  deriving (Show, Eq, Ord, BEncode, FromJSON, ToJSON, Typeable)

nodeIdSize :: Int
nodeIdSize = 20

-- | Meaningless node id, for testing purposes only.
instance Default NodeId where
  def = NodeId (BS.replicate nodeIdSize 0)

instance Serialize NodeId where
  get = NodeId <$> getByteString nodeIdSize
  {-# INLINE get #-}
  put (NodeId bs) = putByteString bs
  {-# INLINE put #-}

-- | ASCII encoded.
instance IsString NodeId where
  fromString str
    | L.length str == nodeIdSize = NodeId (fromString str)
    |           otherwise = error "fromString: invalid NodeId length"
  {-# INLINE fromString #-}

-- | Test if the nth bit is set.
testIdBit :: NodeId -> Word -> Bool
testIdBit (NodeId bs) i
  | fromIntegral i < nodeIdSize * 8
  , (q, r) <- quotRem (fromIntegral i) 8
  = testBit (BS.index bs q) r
  |     otherwise      = False
{-# INLINE testIdBit #-}

-- TODO WARN is the 'system' random suitable for this?
-- | Generate random NodeID used for the entire session.
--   Distribution of ID's should be as uniform as possible.
--
genNodeId :: IO NodeId
genNodeId = NodeId <$> getEntropy nodeIdSize

{-----------------------------------------------------------------------
--  Node address
-----------------------------------------------------------------------}

data NodeAddr a = NodeAddr
  { nodeHost ::                !a
  , nodePort :: {-# UNPACK #-} !PortNumber
  } deriving (Show, Eq)

$(deriveJSON omitRecordPrefix ''NodeAddr)

-- | KRPC compatible encoding.
instance Serialize a => Serialize (NodeAddr a) where
  get = NodeAddr <$> get <*> get
  {-# INLINE get #-}
  put NodeAddr {..} = put nodeHost >> put nodePort
  {-# INLINE put #-}

-- | Torrent file compatible encoding.
instance BEncode a => BEncode (NodeAddr a) where
  toBEncode NodeAddr {..} = toBEncode (nodeHost, nodePort)
  {-# INLINE toBEncode #-}
  fromBEncode b = uncurry NodeAddr <$> fromBEncode b
  {-# INLINE fromBEncode #-}

instance Hashable a => Hashable (NodeAddr a) where
  hashWithSalt s NodeAddr {..} = hashWithSalt s (nodeHost, nodePort)
  {-# INLINE hashWithSalt #-}

-- | Example:
--
--   @nodePort \"127.0.0.1:6881\" == 6881@
--
instance IsString (NodeAddr IPv4) where
  fromString = fromPeerAddr . fromString

fromPeerAddr :: PeerAddr a -> NodeAddr a
fromPeerAddr PeerAddr {..} = NodeAddr
  { nodeHost = peerHost
  , nodePort = peerPort
  }

{-----------------------------------------------------------------------
--  Node info
-----------------------------------------------------------------------}

data NodeInfo a = NodeInfo
  { nodeId   :: !NodeId
  , nodeAddr :: !(NodeAddr a)
  } deriving (Show, Eq)

$(deriveJSON omitRecordPrefix ''NodeInfo)

instance Eq a => Ord (NodeInfo a) where
  compare = comparing nodeId

-- | KRPC 'compact list' compatible encoding: contact information for
-- nodes is encoded as a 26-byte string. Also known as "Compact node
-- info" the 20-byte Node ID in network byte order has the compact
-- IP-address/port info concatenated to the end.
instance Serialize a => Serialize (NodeInfo a) where
  get = NodeInfo <$> get <*> get
  put NodeInfo {..} = put nodeId >> put nodeAddr
{-
type CompactInfo = ByteString

data NodeList a = CompactNodeList [NodeInfo a]

decodeCompact :: Serialize a => CompactInfo -> [NodeInfo a]
decodeCompact = either (const []) id . S.runGet (many get)

encodeCompact :: [NodeId] -> CompactInfo
encodeCompact = S.runPut . mapM_ put

--decodePeerList :: [BEncode] -> [PeerAddr]
--decodePeerList = undefined

--encodePeerList :: [PeerAddr] -> [BEncode]
--encodePeerList = undefined
-}