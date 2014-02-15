-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Every node maintains a routing table of known good nodes. The
--   nodes in the routing table are used as starting points for
--   queries in the DHT. Nodes from the routing table are returned in
--   response to queries from other nodes.
--
--   For more info see:
--   <http://www.bittorrent.org/beps/bep_0005.html#routing-table>
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.BitTorrent.DHT.Routing
       ( -- * Table
         Table

         -- * Attributes
       , BucketCount
       , defaultBucketCount
       , BucketSize
       , defaultBucketSize
       , NodeCount

         -- * Query
       , Network.BitTorrent.DHT.Routing.null
       , Network.BitTorrent.DHT.Routing.full
       , thisId
       , shape
       , Network.BitTorrent.DHT.Routing.size
       , Network.BitTorrent.DHT.Routing.depth

         -- * Lookup
       , K
       , defaultK
       , TableKey (..)
       , kclosest

         -- * Construction
       , Network.BitTorrent.DHT.Routing.nullTable
       , Network.BitTorrent.DHT.Routing.insert

         -- * Conversion
       , Network.BitTorrent.DHT.Routing.TableEntry
       , Network.BitTorrent.DHT.Routing.toList

         -- * Routing
       , Timestamp
       , Routing
       , runRouting
       ) where

import Control.Applicative as A
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

{-----------------------------------------------------------------------
--  Routing monad
-----------------------------------------------------------------------}

-- | Last time the node was responding to our queries.
--
--   Not all nodes that we learn about are equal. Some are \"good\" and
--   some are not. Many nodes using the DHT are able to send queries
--   and receive responses, but are not able to respond to queries
--   from other nodes. It is important that each node's routing table
--   must contain only known good nodes. A good node is a node has
--   responded to one of our queries within the last 15 minutes. A
--   node is also good if it has ever responded to one of our queries
--   and has sent us a query within the last 15 minutes. After 15
--   minutes of inactivity, a node becomes questionable. Nodes become
--   bad when they fail to respond to multiple queries in a row. Nodes
--   that we know are good are given priority over nodes with unknown
--   status.
--
type Timestamp = POSIXTime

-- | Some routing operations might need to perform additional IO.
data Routing ip result
  = Full
  | Done      result
  | GetTime                ( Timestamp    -> Routing ip result)
  | NeedPing (NodeAddr ip) ( Bool         -> Routing ip result)
  | Refresh   NodeId       ([NodeInfo ip] -> Routing ip result)

instance Functor (Routing ip) where
  fmap _  Full             = Full
  fmap f (Done          r) = Done          (     f   r)
  fmap f (GetTime       g) = GetTime       (fmap f . g)
  fmap f (NeedPing addr g) = NeedPing addr (fmap f . g)
  fmap f (Refresh  nid  g) = Refresh  nid  (fmap f . g)

instance Monad (Routing ip) where
  return = Done

  Full         >>= _ = Full
  Done       r >>= m = m r
  GetTime    f >>= m = GetTime     $ \ t -> f t >>= m
  NeedPing a f >>= m = NeedPing a  $ \ p -> f p >>= m
  Refresh  n f >>= m = Refresh  n  $ \ i -> f i >>= m

instance Applicative (Routing ip) where
  pure  = return
  (<*>) = ap

instance Alternative (Routing ip) where
  empty = Full

  Full         <|> m = m
  Done     a   <|> _ = Done a
  GetTime  f   <|> m = GetTime    $ \ t -> f t <|> m
  NeedPing a f <|> m = NeedPing a $ \ p -> f p <|> m
  Refresh  n f <|> m = Refresh  n $ \ i -> f i <|> m

-- | Run routing table operation.
runRouting :: (Monad m, Eq ip)
           => (NodeAddr ip -> m Bool)          -- ^ ping the specific node;
           -> (NodeId      -> m [NodeInfo ip]) -- ^ get closest nodes;
           -> m Timestamp                      -- ^ get current time;
           -> Routing ip f                     -- ^ operation to run;
           -> m (Maybe f)                      -- ^ operation result;
runRouting ping_node find_nodes timestamper = go
  where
    go  Full             = return (Nothing)
    go (Done          r) = return (Just  r)
    go (GetTime       f) = do
      t <- timestamper
      go (f t)

    go (NeedPing addr f) = do
      pong <- ping_node addr
      go (f pong)

    go (Refresh nid f) = do
      infos <- find_nodes nid
      go (f infos)

getTime :: Routing ip Timestamp
getTime = GetTime return
{-# INLINE getTime #-}

needPing :: NodeAddr ip -> Routing ip Bool
needPing addr = NeedPing addr return
{-# INLINE needPing #-}

refresh :: NodeId -> Routing ip [NodeInfo ip]
refresh nid = Refresh nid return
{-# INLINE refresh #-}

{-----------------------------------------------------------------------
    Bucket
-----------------------------------------------------------------------}
-- TODO: add replacement cache to the bucket
--
-- When a k-bucket is full and a new node is discovered for that
-- k-bucket, the least recently seen node in the k-bucket is
-- PINGed. If the node is found to be still alive, the new node is
-- place in a secondary list, a replacement cache. The replacement
-- cache is used only if a node in the k-bucket stops responding. In
-- other words: new nodes are used only when older nodes disappear.

-- | Timestamp - last time this node is pinged.
type NodeEntry ip = Binding (NodeInfo ip) Timestamp

instance (Serialize k, Serialize v) => Serialize (Binding k v) where
  get = (:->) <$> get <*> get
  put (k :-> v) = put k >> put v

-- TODO instance Pretty where

-- | Number of nodes in a bucket.
type BucketSize  = Int

-- | Maximum number of 'NodeInfo's stored in a bucket. Most clients
-- use this value.
defaultBucketSize :: BucketSize
defaultBucketSize = 8

-- | Bucket is also limited in its length — thus it's called k-bucket.
--   When bucket becomes full, we should split it in two lists by
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

-- | Should maintain a set of stable long running nodes.
insertBucket :: Eq ip => Timestamp -> NodeInfo ip -> Bucket ip
           -> ip `Routing` Bucket ip
insertBucket curTime info bucket
  -- just update timestamp if a node is already in bucket
  | Just _ <- PSQ.lookup info bucket = do
    return $ PSQ.insertWith max info curTime bucket

  --  Buckets that have not been changed in 15 minutes should be "refreshed."
  | Just (NodeInfo {..} :-> lastSeen) <- lastChanged bucket
  , curTime - lastSeen > delta = do
    infos   <- refresh nodeId
    refTime <- getTime
    let newBucket = L.foldr (\ x -> PSQ.insertWith max x refTime) bucket infos
    insertBucket refTime info newBucket

  -- If there are any questionable nodes in the bucket have not been
  -- seen in the last 15 minutes, the least recently seen node is
  -- pinged. If any nodes in the bucket are known to have become bad,
  -- then one is replaced by the new node in the next insertBucket
  -- iteration.
  | Just ((old @ NodeInfo {..} :-> leastSeen), rest) <- leastRecently bucket
  , curTime - leastSeen > delta = do
    pong     <- needPing nodeAddr
    pongTime <- getTime
    let newBucket = if pong then PSQ.insert old pongTime bucket else rest
    insertBucket pongTime info newBucket

  -- bucket is good, but not full => we can insert a new node
  | PSQ.size bucket < defaultBucketSize = do
    return $ PSQ.insert info curTime bucket

  -- When the bucket is full of good nodes, the new node is simply discarded.
  | otherwise = A.empty

insertNode :: Eq ip => NodeInfo ip -> Bucket ip -> ip `Routing` Bucket ip
insertNode info bucket = do
  curTime <- getTime
  insertBucket curTime info bucket

type BitIx = Word

split :: Eq ip => BitIx -> Bucket ip -> (Bucket ip, Bucket ip)
split i = (PSQ.fromList *** PSQ.fromList) . partition spanBit . PSQ.toList
  where
    spanBit entry = testIdBit (nodeId (key entry)) i

{-----------------------------------------------------------------------
--  Table
-----------------------------------------------------------------------}

-- | Number of buckets in a routing table.
type BucketCount = Int

defaultBucketCount :: BucketCount
defaultBucketCount = 20

-- | The routing table covers the entire 'NodeId' space from 0 to 2 ^
-- 160. The routing table is subdivided into 'Bucket's that each cover
-- a portion of the space. An empty table has one bucket with an ID
-- space range of @min = 0, max = 2 ^ 160@. When a node with ID \"N\"
-- is inserted into the table, it is placed within the bucket that has
-- @min <= N < max@. An empty table has only one bucket so any node
-- must fit within it. Each bucket can only hold 'K' nodes, currently
-- eight, before becoming 'Full'. When a bucket is full of known good
-- nodes, no more nodes may be added unless our own 'NodeId' falls
-- within the range of the 'Bucket'. In that case, the bucket is
-- replaced by two new buckets each with half the range of the old
-- bucket and the nodes from the old bucket are distributed among the
-- two new ones. For a new table with only one bucket, the full bucket
-- is always split into two new buckets covering the ranges @0..2 ^
-- 159@ and @2 ^ 159..2 ^ 160@.
--
data Table ip
  -- most nearest bucket
  = Tip  NodeId BucketCount (Bucket ip)

  -- left biased tree branch
  | Zero (Table  ip) (Bucket ip)

  -- right biased tree branch
  | One  (Bucket ip) (Table  ip)
    deriving (Show, Generic)

instance Eq ip => Eq (Table ip) where
  (==) = (==) `on` Network.BitTorrent.DHT.Routing.toList

instance Serialize NominalDiffTime where
  put = putWord32be . fromIntegral   . fromEnum
  get = (toEnum     . fromIntegral) <$> getWord32be

-- | Normally, routing table should be saved between invocations of
-- the client software. Note that you don't need to store /this/
-- 'NodeId' since it is already included in routing table.
instance (Eq ip, Serialize ip) => Serialize (Table ip)

-- | Shape of the table.
instance Pretty (Table ip) where
  pretty t
    | bucketCount < 6 = hcat $ punctuate ", " $ L.map PP.int ss
    |    otherwise    = brackets $
      PP.int (L.sum    ss) <> " nodes, " <>
      PP.int bucketCount   <> " buckets"
    where
      bucketCount = L.length ss
      ss = shape t

-- | Empty table with specified /spine/ node id.
nullTable :: Eq ip => NodeId -> BucketCount -> Table ip
nullTable nid n = Tip nid (bucketCount (pred n)) PSQ.empty
  where
    bucketCount x = max 0 (min 159 x)

-- | Test if table is empty. In this case DHT should start
-- bootstrapping process until table becomes 'full'.
null :: Table ip -> Bool
null (Tip _ _ b) = PSQ.null b
null  _          = False

-- | Test if table have maximum number of nodes. No more nodes can be
-- 'insert'ed, except old ones becomes bad.
full :: Table ip -> Bool
full (Tip  _ n _) = n == 0
full (Zero   t b) = PSQ.size b == defaultBucketSize && full t
full (One    b t) = PSQ.size b == defaultBucketSize && full t

-- | Get the /spine/ node id.
thisId :: Table ip -> NodeId
thisId (Tip  nid _ _) = nid
thisId (Zero table _) = thisId table
thisId (One _  table) = thisId table

-- | Number of nodes in a bucket or a table.
type NodeCount   = Int

-- | Internally, routing table is similar to list of buckets or a
-- /matrix/ of nodes. This function returns the shape of the matrix.
shape :: Table ip -> [BucketSize]
shape (Tip _ _ bucket) = [PSQ.size bucket]
shape (Zero t  bucket) = PSQ.size bucket : shape t
shape (One bucket t  ) = PSQ.size bucket : shape t

-- | Get number of nodes in the table.
size :: Table ip -> NodeCount
size = L.sum . shape

-- | Get number of buckets in the table.
depth :: Table ip -> BucketCount
depth = L.length . shape

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

-- | Count of closest nodes in find_node request.
type K = Int

-- | Default 'K' is equal to 'defaultBucketSize'.
defaultK :: K
defaultK = 8

class TableKey k where
  toNodeId :: k -> NodeId

instance TableKey NodeId where
  toNodeId = id

instance TableKey InfoHash where
  toNodeId = either (error msg) id . S.decode . S.encode
    where -- TODO unsafe coerse?
      msg = "tableKey: impossible"

-- | Get a list of /K/ closest nodes using XOR metric. Used in
-- 'find_node' and 'get_peers' queries.
kclosest :: Eq ip => TableKey a => K -> a -> Table ip -> [NodeInfo ip]
kclosest k (toNodeId -> nid)
  = L.take k . rank nid
  . L.map PSQ.key . PSQ.toList . fromMaybe PSQ.empty
  . lookupBucket nid

{-----------------------------------------------------------------------
--  Routing
-----------------------------------------------------------------------}

splitTip :: Eq ip => NodeId -> BucketCount -> BitIx -> Bucket ip -> Table ip
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
    go i (Tip nid n bucket)
      |        n == 0       =   Tip nid n     <$> insertNode info bucket
      |     otherwise       =   Tip nid n     <$> insertNode info bucket
                           <|>  go (succ i) (splitTip nid n i bucket)

{-----------------------------------------------------------------------
--  Conversion
-----------------------------------------------------------------------}

type TableEntry ip = (NodeInfo ip, Timestamp)

tableEntry :: NodeEntry ip -> TableEntry ip
tableEntry (a :-> b) = (a, b)

-- | Non-empty list of buckets.
toBucketList :: Table ip -> [Bucket ip]
toBucketList (Tip _ _ b) = [b]
toBucketList (Zero  t b) = b : toBucketList t
toBucketList (One   b t) = b : toBucketList t

toList :: Eq ip => Table ip -> [[TableEntry ip]]
toList = L.map (L.map tableEntry . PSQ.toList) . toBucketList
