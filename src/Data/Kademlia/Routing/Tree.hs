-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Routing tree should contain key -> value pairs in this way:
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
module Data.Kademlia.Routing.Tree
       ( Tree, empty, insert
       ) where

import Control.Applicative hiding (empty)
import Data.Bits

import Data.Kademlia.Routing.Bucket (Bucket, split, isFull)
import qualified Data.Kademlia.Routing.Bucket as Bucket



data Tree k v
  = Tip (Bucket k v)
  | Bin (Tree k v)   (Bucket k v)

empty :: Int -> Tree k v
empty = Tip . Bucket.empty

insert :: Applicative f
       => Bits k
       => (v -> f Bool)
       -> (k, v) -> Tree k v -> f (Tree k v)
insert ping (k, v) = go 0
  where
    go n (Tip bucket)
      | isFull bucket, (near, far) <- split n bucket
                          = pure (Tip near `Bin` far)
      |     otherwise     = Tip <$> Bucket.insert ping (k, v) bucket

    go n (Bin near far)
      | k `testBit` n = Bin <$> pure near <*> Bucket.insert ping (k, v) far
      | otherwise     = Bin <$> go (succ n) near <*> pure far
