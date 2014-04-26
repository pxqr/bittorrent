module Network.BitTorrent.DHT.ContactInfo
       ( PeerStore
       , Network.BitTorrent.DHT.ContactInfo.lookup
       , Network.BitTorrent.DHT.ContactInfo.insert
       ) where

import Data.Default
import Data.List as L
import Data.Maybe
import Data.Monoid
import Data.HashMap.Strict as HM
import Data.Serialize

import Data.Torrent
import Network.BitTorrent.Address

{-
import Data.HashMap.Strict as HM

import Data.Torrent.InfoHash
import Network.BitTorrent.Address

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

-- | Storage used to keep track a set of known peers in client,
-- tracker or DHT sessions.
newtype PeerStore ip = PeerStore (HashMap InfoHash [PeerAddr ip])

-- | Empty store.
instance Default (PeerStore a) where
  def = PeerStore HM.empty
  {-# INLINE def #-}

-- | Monoid under union operation.
instance Eq a => Monoid (PeerStore a) where
  mempty  = def
  {-# INLINE mempty #-}

  mappend (PeerStore a) (PeerStore b) =
    PeerStore (HM.unionWith L.union a b)
  {-# INLINE mappend #-}

-- | Can be used to store peers between invocations of the client
-- software.
instance Serialize (PeerStore a) where
  get = undefined
  put = undefined

-- | Used in 'get_peers' DHT queries.
lookup :: InfoHash -> PeerStore a -> [PeerAddr a]
lookup ih (PeerStore m) = fromMaybe [] $ HM.lookup ih m

-- | Used in 'announce_peer' DHT queries.
insert :: Eq a => InfoHash -> PeerAddr a -> PeerStore a -> PeerStore a
insert ih a (PeerStore m) = PeerStore (HM.insertWith L.union ih [a] m)
