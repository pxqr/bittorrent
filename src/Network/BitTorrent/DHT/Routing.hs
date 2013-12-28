-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.BitTorrent.DHT.Routing
       ( -- * Routing table
         Table

         -- * Routing
       , Timestamp
       , Routing
       , runRouting

         -- * Query
       , thisId
       , Network.BitTorrent.DHT.Routing.size
       , Network.BitTorrent.DHT.Routing.depth
       , K
       , Network.BitTorrent.DHT.Routing.kclosest
       , Network.BitTorrent.DHT.Routing.kclosestHash

         -- * Construction
       , Network.BitTorrent.DHT.Routing.nullTable
       , Network.BitTorrent.DHT.Routing.insert
       ) where

import Control.Applicative hiding (empty)
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List as L hiding (insert)
import Data.Maybe
import Data.Monoid
import Data.PSQueue as PSQ
import Data.Serialize as S hiding (Result, Done)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import GHC.Generics
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class

import Data.Torrent.InfoHash
import Network.BitTorrent.Core

{-
-- | Routing tree should contain key -> value pairs in this way:
--
--     * More keys that near to our node key, and less keys that far
--     from our node key.
--
--     * Tree might be saturated. If this happen we can only update
--     buckets, but we can't add new buckets.
--
--   Instead of using ordinary binary tree and keep track is it
--   following restrictions above (that's somewhat non-trivial) we
--   store distance -> value keys. This lead to simple data structure
--   that actually isomorphic to non-empty list. So we first map our
--   keys to distances using our node ID and store them in tree. When
--   we need to extract a pair we map distances to keys back, again
--   using our node ID. This normalization happen in routing table.
--
data Tree k v
  = Tip (Bucket k v)
  | Bin (Tree k v)   (Bucket k v)

empty :: Int -> Tree k v
empty = Tip . Bucket.empty

insert :: Applicative f => Bits k
       => (v -> f Bool) -> (k, v) -> Tree k v -> f (Tree k v)
insert ping (k, v) = go 0
  where
    go n (Tip bucket)
      | isFull bucket, (near, far) <- split n bucket
                          = pure (Tip near `Bin` far)
      |     otherwise     = Tip <$> Bucket.insert ping (k, v) bucket

    go n (Bin near far)
      | k `testBit` n = Bin <$> pure near <*> Bucket.insert ping (k, v) far
      | otherwise     = Bin <$> go (succ n) near <*> pure far
-}

{-----------------------------------------------------------------------
--  Insertion
-----------------------------------------------------------------------}

type Timestamp = POSIXTime

data Routing ip result
  = Full      result
  | Done     (Timestamp -> result)
  | GetTime                ( Timestamp                 -> Routing ip result)
  | Refresh   NodeId       (([NodeInfo ip], Timestamp) -> Routing ip result)
  | NeedPing (NodeAddr ip) (Maybe Timestamp            -> Routing ip result)

instance Functor (Routing ip) where
  fmap f (Full          r) = Full          (     f   r)
  fmap f (Done          r) = Done          (     f . r)
  fmap f (GetTime       g) = GetTime       (fmap f . g)
  fmap f (Refresh  addr g) = Refresh  addr (fmap f . g)
  fmap f (NeedPing addr g) = NeedPing addr (fmap f . g)

runRouting :: (Monad m, Eq ip)
             => (NodeAddr ip -> m Bool)          -- ^ ping_node
             -> (NodeId      -> m [NodeInfo ip]) -- ^ find_nodes
             -> m Timestamp                      -- ^ timestamper
             -> Routing ip f                     -- ^ action
             -> m f                              -- ^ result
runRouting ping_node find_nodes timestamper = go
  where
    go (Full          r) = return r
    go (Done          f) = liftM f timestamper
    go (GetTime       f) = do
      t <- timestamper
      go (f t)

    go (NeedPing addr f) = do
      pong <- ping_node addr
      if pong
        then do
             time <- timestamper
             go (f (Just time))
        else go (f Nothing)

    go (Refresh nid f) = do
      infos <- find_nodes nid
      time  <- timestamper
      go (f (infos, time))

{-----------------------------------------------------------------------
    Bucket
-----------------------------------------------------------------------}

-- | Timestamp - last time this node is pinged.
type NodeEntry ip = Binding (NodeInfo ip) Timestamp

instance (Serialize k, Serialize v) => Serialize (Binding k v) where
  get = (:->) <$> get <*> get
  put (k :-> v) = put k >> put v

-- TODO instance Pretty where

-- | Most clients use this value for maximum bucket size.
defaultBucketSize :: Int
defaultBucketSize = 20

-- | Bucket is also limited in its length — thus it's called k-bucket.
--   When bucket becomes full we should split it in two lists by
--   current span bit. Span bit is defined by depth in the routing
--   table tree. Size of the bucket should be choosen such that it's
--   very unlikely that all nodes in bucket fail within an hour of
--   each other.
--
type Bucket ip = PSQ (NodeInfo ip) Timestamp

instance (Serialize k, Serialize v, Ord k, Ord v)
       => Serialize (PSQ k v) where
  get = PSQ.fromList <$> get
  put = put . PSQ.toList

-- | Get the most recently changed node entry, if any.
lastChanged :: Eq ip => Bucket ip -> Maybe (NodeEntry ip)
lastChanged bucket
  | L.null timestamps = Nothing
  |      otherwise    = Just (L.maximumBy (compare `on` prio) timestamps)
  where
    timestamps = PSQ.toList bucket

leastRecently :: Eq ip => Bucket ip -> Maybe (NodeEntry ip, Bucket ip)
leastRecently = minView

-- | Update interval, in seconds.
delta :: NominalDiffTime
delta = 15 * 60

-- | Max bucket size, in nodes.
type Alpha = Int

defaultAlpha :: Int
defaultAlpha = 8

insertBucket :: Eq ip => Timestamp -> NodeInfo ip -> Bucket ip
           -> ip `Routing` Bucket ip
insertBucket curTime info bucket
  -- just update timestamp if a node is already in bucket
  | Just _ <- PSQ.lookup info bucket
  = Done $ \ t -> PSQ.insertWith max info t bucket

  -- update the all bucket if it is too outdated
  | Just (NodeInfo {..} :-> lastSeen) <- lastChanged bucket
  , curTime - lastSeen > delta
  = Refresh nodeId $ \ (infos, t) ->
      insertNode info $
      L.foldr (\ x -> PSQ.insertWith max x t) bucket infos

  -- update questionable nodes, if any; then try to insert our new node
  -- this case can remove bad nodes from bucket, so we can insert a new one
  | Just ((old @ NodeInfo {..} :-> leastSeen), rest) <- leastRecently bucket
  , curTime - leastSeen > delta
  = NeedPing nodeAddr $ \ mtime ->
      insertNode info $
        case mtime of
          Nothing       -> bucket
          Just pongTime -> PSQ.insert old pongTime bucket

  -- bucket is good, but not full => we can insert a new node
  | PSQ.size bucket < defaultAlpha  = Done (\ t -> PSQ.insert info t bucket)

  -- bucket is full of good nodes => ignore new node
  | otherwise = Full bucket

insertNode :: Eq ip => NodeInfo ip -> Bucket ip -> ip `Routing` Bucket ip
insertNode info bucket = GetTime $ \ curTime -> insertBucket curTime info bucket

type BitIx = Word

split :: Eq ip => BitIx -> Bucket ip -> (Bucket ip, Bucket ip)
split i = (PSQ.fromList *** PSQ.fromList) . partition spanBit . PSQ.toList
  where
    spanBit entry = testIdBit (nodeId (key entry)) i

{-----------------------------------------------------------------------
--  Table
-----------------------------------------------------------------------}

defaultBucketCount :: Int
defaultBucketCount = 20

data Table ip
  = Tip  NodeId Int (Bucket ip)
  | Zero (Table  ip) (Bucket ip)
  | One  (Bucket ip) (Table  ip)
    deriving Generic

instance Serialize NominalDiffTime where
  put = putWord32be . fromIntegral   . fromEnum
  get = (toEnum     . fromIntegral) <$> getWord32be

-- | Normally, routing table should we saved between invocations of
-- the client software. Note that you don't need store /this/ 'NodeId'
-- since it is included in routing table.
instance (Eq ip, Serialize ip) => Serialize (Table ip)

instance Pretty (Table ip) where
  pretty t =
      "size  = " <> PP.int (Network.BitTorrent.DHT.Routing.size  t) <>
    ", depth = " <> PP.int (depth t)


nullTable :: Eq ip => NodeId -> Table ip
nullTable nid = Tip nid defaultBucketCount PSQ.empty

thisId :: Table ip -> NodeId
thisId (Tip  nid _ _) = nid
thisId (Zero table _) = thisId table
thisId (One _  table) = thisId table

-- | Get number of nodes in the table.
size :: Table ip -> Int
size = go
  where
    go (Tip _ _ bucket) = PSQ.size bucket
    go (Zero t  bucket) = go t + PSQ.size bucket
    go (One bucket t  ) = PSQ.size bucket + go t

-- | Get number of buckets in the table.
depth :: Table ip -> Int
depth = go
  where
    go (Tip _ _ _) = 1
    go (Zero t  _) = succ (go t)
    go (One  _  t) = succ (go t)

lookupBucket :: NodeId -> Table ip -> Maybe (Bucket ip)
lookupBucket nid = go 0
  where
    go i (Zero table bucket)
      |  testIdBit nid i  = pure bucket
      |     otherwise     = go (succ i) table
    go i (One  bucket table)
      |  testIdBit nid i  = go (succ i) table
      |     otherwise     = pure bucket
    go _ (Tip _ _ bucket) = pure bucket

type K = Int

-- | Used in 'find_node' queries.
kclosest :: Eq ip => K -> NodeId -> Table ip -> [NodeInfo ip]
kclosest k nid = L.map key . PSQ.toList . fromMaybe PSQ.empty
               . lookupBucket nid

coerseId :: (Serialize a, Serialize b) => a -> b
coerseId = either (error msg) id . S.decode . S.encode
  where
    msg = "coerseId: impossible"

-- | Used in 'get_peers' queries.
kclosestHash :: Eq a => Alpha -> InfoHash -> Table a -> [NodeInfo a]
kclosestHash k nid t = kclosest k (coerseId nid) t

{-----------------------------------------------------------------------
--  Routing
-----------------------------------------------------------------------}

splitTip :: Eq ip => NodeId -> Int -> BitIx -> Bucket ip -> Table ip
splitTip nid n i bucket
  | testIdBit nid i = (One  zeros (Tip nid (pred n) ones))
  |    otherwise    = (Zero (Tip nid (pred n) zeros) ones)
  where
    (zeros, ones) = split i bucket

-- | Used in each query.
insert :: Eq ip => NodeInfo ip -> Table ip -> ip `Routing` Table ip
insert info @ NodeInfo {..} = go (0 :: BitIx)
  where
    go i (Zero table  bucket)
      | testIdBit nodeId i  =   Zero  table   <$> insertNode info bucket
      |     otherwise       = (`Zero` bucket) <$> go (succ i) table
    go i (One  bucket table )
      | testIdBit nodeId i  =   One  bucket   <$> go (succ i) table
      |     otherwise       = (`One` table)   <$> insertNode info bucket
    go i (Tip nid n bucket) = case insertNode info bucket of
      Full kbucket
        |   n == 0  -> Tip nid n <$> Full kbucket
        | otherwise -> go (succ i) (splitTip nid n i kbucket)
      result -> Tip nid n <$> result
