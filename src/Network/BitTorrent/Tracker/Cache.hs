-- |
--   Copyright   :  (c) Sam Truzjan 2014
--   License     :  BSD
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Cached data for tracker responses.
--
module Network.BitTorrent.Tracker.Cache
       ( -- * Cache
         Cached
       , lastUpdated
       , updateInterval
       , minUpdateInterval

         -- * Construction
       , newCached
       , newCached_

         -- * Query
       , isAlive
       , isStalled
       , isExpired
       , canUpdate
       , shouldUpdate

         -- * Cached data
       , tryTakeData
       , takeData
       ) where

import Control.Applicative
import Data.Monoid
import Data.Default
import Data.Time
import Data.Time.Clock.POSIX


data Cached a = Cached
  { -- | Time of resource creation.
    lastUpdated           :: !POSIXTime

    -- | Minimum invalidation timeout.
  , minUpdateInterval     :: !NominalDiffTime

    -- | Resource lifetime.
  , updateInterval        :: !NominalDiffTime

    -- | Resource data.
  , cachedData            :: a
  } deriving (Show, Eq)

-- INVARIANT: minUpdateInterval <= updateInterval

instance Default (Cached a) where
  def = mempty

instance Functor Cached where
  fmap f (Cached t i m a) = Cached t i m (f a)

posixEpoch :: NominalDiffTime
posixEpoch = 1000000000000000000000000000000000000000000000000000000

instance Applicative Cached where
  pure = Cached 0 posixEpoch posixEpoch
  f <*> c = Cached
    { lastUpdated       = undefined
    , minUpdateInterval = undefined
    , updateInterval    = undefined
    , cachedData        = cachedData f (cachedData c)
    }

instance Alternative Cached where
  empty = mempty
  (<|>) = error "cached alternative instance: not implemented"

instance Monad Cached where
  return = pure
  Cached {..} >>= f = Cached
    { lastUpdated       = undefined
    , updateInterval    = undefined
    , minUpdateInterval = undefined
    , cachedData        = undefined
    }

instance Monoid (Cached a) where
  mempty = Cached
    { lastUpdated       = 0
    , minUpdateInterval = 0
    , updateInterval    = 0
    , cachedData        = error "cached mempty: impossible happen"
    }

  mappend a b
    | expirationTime a > expirationTime b = a
    |             otherwise               = b

normalize :: NominalDiffTime -> NominalDiffTime
          -> (NominalDiffTime, NominalDiffTime)
normalize a b
  |   a < b   = (a, b)
  | otherwise = (b, a)
{-# INLINE normalize #-}

newCached :: NominalDiffTime -> NominalDiffTime -> a -> IO (Cached a)
newCached minInterval interval x = do
  t <- getPOSIXTime
  let (mui, ui) = normalize minInterval interval
  return Cached
    { lastUpdated       = t
    , minUpdateInterval = mui
    , updateInterval    = ui
    , cachedData        = x
    }

newCached_ :: NominalDiffTime -> a -> IO (Cached a)
newCached_ interval x = newCached interval interval x
{-# INLINE newCached_ #-}

expirationTime :: Cached a -> POSIXTime
expirationTime Cached {..} = undefined

isAlive :: Cached a -> IO Bool
isAlive Cached {..} = do
  currentTime <- getPOSIXTime
  return $ lastUpdated + updateInterval > currentTime

isExpired :: Cached a -> IO Bool
isExpired Cached {..} = undefined

isStalled :: Cached a -> IO Bool
isStalled Cached {..} = undefined

canUpdate :: Cached a -> IO (Maybe NominalDiffTime)
canUpdate = undefined --isStaled

shouldUpdate :: Cached a -> IO (Maybe NominalDiffTime)
shouldUpdate = undefined -- isExpired

tryTakeData :: Cached a -> IO (Maybe a)
tryTakeData c = do
  alive <- isAlive c
  return $ if alive then Just (cachedData c) else Nothing

invalidateData :: Cached a -> IO a -> IO (Cached a)
invalidateData Cached {..} action = do
  t <- getPOSIXTime
  x <- action
  return Cached
    { lastUpdated       = t
    , updateInterval    = updateInterval
    , minUpdateInterval = minUpdateInterval
    , cachedData        = x
    }

takeData :: Cached a -> IO a -> IO a
takeData c action = do
  mdata <- tryTakeData c
  case mdata of
    Just  a -> return a
    Nothing -> do
      c' <- invalidateData c action
      takeData c' action
