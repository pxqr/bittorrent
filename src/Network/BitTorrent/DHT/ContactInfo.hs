module Network.BitTorrent.DHT.ContactInfo
       (        ) where
{-
import Data.HashMap.Strict as HM

import Data.Torrent.InfoHash
import Network.BitTorrent.Core

-- increase prefix when table is too large
-- decrease prefix when table is too small
-- filter outdated peers

{-----------------------------------------------------------------------
--  PeerSet
-----------------------------------------------------------------------}

type PeerSet a = [(PeerAddr a, NodeInfo a, Timestamp)]

-- compare PSQueue vs Ordered list

takeNewest :: PeerSet a -> [PeerAddr a]
takeNewest = undefined

dropOld :: Timestamp -> PeerSet a -> PeerSet a
dropOld = undefined

insert :: PeerAddr a -> Timestamp -> PeerSet a -> PeerSet a
insert = undefined

type Mask = Int
type Size = Int
type Timestamp = Int

{-----------------------------------------------------------------------
--  InfoHashMap
-----------------------------------------------------------------------}

-- compare handwritten prefix tree versus IntMap

data Tree a
  = Nil
  | Tip !InfoHash !(PeerSet a)
  | Bin !InfoHash !Mask !Size !Timestamp (Tree a) (Tree a)

insertTree :: InfoHash -> a -> Tree a -> Tree a
insertTree = undefined

type Prio = Int

--shrink :: ContactInfo ip -> Int
shrink  Nil      = Nil
shrink (Tip _ _) = undefined
shrink (Bin _ _) = undefined

{-----------------------------------------------------------------------
-- InfoHashMap
-----------------------------------------------------------------------}

-- compare new design versus HashMap

data IntMap k p a
type ContactInfo = Map InfoHash Timestamp (Set (PeerAddr IP) Timestamp)

data ContactInfo ip = PeerStore
  { maxSize    :: Int
  , prefixSize :: Int
  , thisNodeId :: NodeId

  , count      :: Int  -- ^ Cached size of the 'peerSet'
  , peerSet    :: HashMap InfoHash [PeerAddr ip]
  }

size :: ContactInfo ip -> Int
size = undefined

prefixSize :: ContactInfo ip -> Int
prefixSize = undefined

lookup :: InfoHash -> ContactInfo ip -> [PeerAddr ip]
lookup = undefined

insert :: InfoHash -> PeerAddr ip -> ContactInfo ip -> ContactInfo ip
insert = undefined

-- | Limit in size.
prune :: NodeId -> Int -> ContactInfo ip -> ContactInfo ip
prune pref targetSize  Nil      = Nil
prune pref targetSize (Tip _ _) = undefined

-- | Remove expired entries.
splitGT :: Timestamp -> ContactInfo ip -> ContactInfo ip
splitGT = undefined
-}