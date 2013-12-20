{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Ord
import Data.Serialize as S
import Data.Word
import Network
import System.Entropy

import Data.Torrent.JSON
import Network.BitTorrent.Core.PeerAddr ()

{-----------------------------------------------------------------------
--  Node id
-----------------------------------------------------------------------}
-- TODO more compact representation ('ShortByteString's?)

-- | Normally, /this/ node id should we saved between invocations of
-- the client software.
newtype NodeId = NodeId ByteString
  deriving (Show, Eq, Ord, BEncode, FromJSON, ToJSON)

nodeIdSize :: Int
nodeIdSize = 20

instance Serialize NodeId where
  get = NodeId <$> getByteString nodeIdSize
  {-# INLINE get #-}
  put (NodeId bs) = putByteString bs
  {-# INLINE put #-}

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
  put NodeAddr {..} = put nodeHost >> put nodePort

-- | Torrent file compatible encoding.
instance BEncode a => BEncode (NodeAddr a) where
  toBEncode NodeAddr {..} = toBEncode (nodeHost, nodePort)
  {-# INLINE toBEncode #-}
  fromBEncode b = uncurry NodeAddr <$> fromBEncode b
  {-# INLINE fromBEncode #-}

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

-- | KRPC 'compact list' compatible encoding.
instance Serialize a => Serialize (NodeInfo a) where
  get = NodeInfo <$> get <*> get
  put NodeInfo {..} = put nodeId >> put nodeAddr

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
