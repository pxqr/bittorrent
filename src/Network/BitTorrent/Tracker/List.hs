-- |
--   Copyright   :  (c) Sam Truzjan 2014
--   License     :  BSD
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Multitracker Metadata Extension support.
--
--   For more info see: <http://www.bittorrent.org/beps/bep_0012.html>
--
{-# LANGUAGE FlexibleInstances #-}
module Network.BitTorrent.Tracker.List
       ( -- * Tracker list
         TierEntry
       , TrackerList

         -- * Construction
       , trackerList
       , shuffleTiers
       , mapWithURI
       , Network.BitTorrent.Tracker.List.toList

         -- * Traversals
       , traverseAll
       , traverseTiers
       ) where

import Prelude hiding (mapM, foldr)
import Control.Arrow
import Control.Applicative
import Control.Exception
import Data.Default
import Data.List as L (map, elem, any, filter, null)
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Network.URI
import System.Random.Shuffle

import Data.Torrent
import Network.BitTorrent.Tracker.RPC as RPC

{-----------------------------------------------------------------------
--  Tracker list datatype
-----------------------------------------------------------------------}

type TierEntry a = (URI, a)
type Tier a = [TierEntry a]

-- | Tracker list is either a single tracker or list of tiers. All
-- trackers in each tier must be checked before the client goes on to
-- the next tier.
data TrackerList a
  = Announce (TierEntry a) -- ^ torrent file 'announce'      field only
  | TierList [Tier a]      -- ^ torrent file 'announce-list' field only
    deriving (Show, Eq)

-- | Empty tracker list. Can be used for trackerless torrents.
instance Default (TrackerList a) where
  def = TierList []

instance Functor TrackerList where
  fmap f (Announce (uri, a)) = Announce (uri, f a)
  fmap f (TierList a)        = TierList (fmap (fmap (second f)) a)

instance Foldable TrackerList where
  foldr f z (Announce e ) = f (snd e) z
  foldr f z (TierList xs) = foldr (flip (foldr (f . snd))) z xs

_traverseEntry f (uri, a) = (,) uri <$> f a

instance Traversable TrackerList where
  traverse f (Announce e ) = Announce <$> _traverseEntry f e
  traverse f (TierList xs) =
      TierList <$> traverse (traverse (_traverseEntry f)) xs

traverseWithURI :: Applicative f
                => (TierEntry a -> f b) -> TrackerList a -> f (TrackerList b)
traverseWithURI f (Announce (uri, a)) = (Announce . (,) uri) <$> f (uri, a)
traverseWithURI f (TierList  xxs    ) =
    TierList <$> traverse (traverse (traverseEntry f)) xxs
  where
    traverseEntry f (uri, a) = (,) uri <$> f (uri, a)

{-----------------------------------------------------------------------
--  List extraction
-----------------------------------------------------------------------}
-- BEP12 do not expose any restrictions for the content of
-- 'announce-list' key - there are some /bad/ cases can happen with
-- poorly designed or even malicious torrent creation software.
--
-- Bad case #1: announce-list is present, but empty.
--
--   { tAnnounce     = Just "http://a.com"
--   , tAnnounceList = Just [[]]
--   }
--
-- Bad case #2: announce uri do not present in announce list.
--
--   { tAnnounce     = Just "http://a.com"
--   , tAnnounceList = Just [["udp://a.com"]]
--   }
--
--  The addBackup function solves both problems by adding announce uri
--  as backup tier.
--
addBackup :: [[URI]] -> URI -> [[URI]]
addBackup tiers bkp
  | L.any (L.elem bkp) tiers = tiers
  |         otherwise        = tiers ++ [[bkp]]

fixList :: Maybe [[URI]] -> Maybe URI -> Maybe [[URI]]
fixList mxss mx = do
  xss <- mxss
  let xss' = L.filter (not . L.null) xss
  return $ maybe xss' (addBackup xss') mx

-- | Extract set of trackers from torrent file. The 'tAnnounce' key is
-- only ignored if the 'tAnnounceList' key is present.
trackerList :: Torrent -> TrackerList ()
trackerList Torrent {..} = fromMaybe (TierList []) $ do
       (TierList . tierList)  <$> (tAnnounceList `fixList` tAnnounce)
   <|> (Announce . nullEntry) <$>  tAnnounce
  where
    nullEntry uri = (uri, ())
    tierList      = L.map (L.map nullEntry)

-- | Shuffle /order of trackers/ in each tier, preserving original
-- /order of tiers/. This can help to balance the load between the
-- trackers.
shuffleTiers :: TrackerList a -> IO (TrackerList a)
shuffleTiers (Announce a ) = return (Announce a)
shuffleTiers (TierList xs) = TierList <$> mapM shuffleM xs

mapWithURI :: (URI -> a -> b) -> TrackerList a -> TrackerList b
mapWithURI f (Announce (uri, a)) = Announce (uri, f uri a)
mapWithURI f (TierList xs      ) = TierList (L.map (L.map mapEntry) xs)
  where
    mapEntry (uri, a) = (uri, f uri a)

toList :: TrackerList a -> [[TierEntry a]]
toList (Announce   e) = [[e]]
toList (TierList xxs) = xxs

{-----------------------------------------------------------------------
--  Special traversals (suppressed RPC exceptions)
-----------------------------------------------------------------------}

catchRPC :: IO a -> IO a -> IO a
catchRPC a b = catch a (f b)
  where
    f :: a -> RpcException -> a
    f = const

throwRPC :: String -> IO a
throwRPC = throwIO . GenericException

-- | Like 'traverse' but ignores 'RpcExceptions'.
traverseAll :: (TierEntry a -> IO a) -> TrackerList a -> IO (TrackerList a)
traverseAll action = traverseWithURI (action $?)
  where
    f $? x = catchRPC (f x) (return (snd x))

-- | Like 'traverse' but put working trackers to the head of tiers.
-- This can help to avoid exceessive requests to not available
-- trackers at each reannounce. If no one action succeed then original
-- list is returned.
traverseTiers :: (TierEntry a -> IO a) -> TrackerList a -> IO (TrackerList a)
traverseTiers action ts = catchRPC (goList ts) (return ts)
  where
    goList tl @ (Announce _    ) = traverseWithURI action tl
    goList      (TierList tiers) = TierList <$> goTiers (goTier []) tiers

    goTiers _ []       = throwRPC "traverseTiers: no tiers"
    goTiers f (x : xs) = catchRPC shortcut failback
      where
        shortcut = do
          x' <- f x
          return (x' : xs)

        failback = do
          xs' <- goTiers f xs
          return (x : xs')

    goTier _      []       = throwRPC "traverseTiers: no trackers in tier"
    goTier failed ((uri, a) : as) = catchRPC shortcut failback
      where
        shortcut = do
          a' <- action (uri, a)
          return ((uri, a') : as ++ failed) -- failed trackers at the end

        failback = goTier ((uri, a) : failed) as
