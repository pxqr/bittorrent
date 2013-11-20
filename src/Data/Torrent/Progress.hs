-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   'Progress' used to track amount downloaded\/left\/upload bytes
--   either on per client or per torrent basis. This value is used to
--   notify the tracker and usually shown to the user. To aggregate
--   total progress you can use the Monoid instance.
--
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module Data.Torrent.Progress
       ( -- * Progress
         Progress (..)

         -- * Lens
       , left
       , uploaded
       , downloaded

         -- * Construction
       , startProgress
       , downloadedProgress
       , enqueuedProgress
       , uploadedProgress
       , dequeuedProgress
       ) where

import Control.Applicative
import Control.Lens
import Data.Aeson.TH
import Data.Default
import Data.List as L
import Data.Monoid
import Data.Serialize as S
import Data.Word


-- | Progress data is considered as dynamic within one client
-- session. This data also should be shared across client application
-- sessions (e.g. files), otherwise use 'startProgress' to get initial
-- 'Progress' value.
--
data Progress = Progress
  { _downloaded :: {-# UNPACK #-} !Word64 -- ^ Total amount of bytes downloaded;
  , _left       :: {-# UNPACK #-} !Word64 -- ^ Total amount of bytes left;
  , _uploaded   :: {-# UNPACK #-} !Word64 -- ^ Total amount of bytes uploaded.
  } deriving (Show, Read, Eq)

$(makeLenses ''Progress)
$(deriveJSON L.tail ''Progress)

instance Serialize Progress where
  put Progress {..} = do
    putWord64be $ fromIntegral _downloaded
    putWord64be $ fromIntegral _left
    putWord64be $ fromIntegral _uploaded

  get = Progress
    <$> (fromIntegral <$> getWord64be)
    <*> (fromIntegral <$> getWord64be)
    <*> (fromIntegral <$> getWord64be)

instance Default Progress where
  def = Progress 0 0 0
  {-# INLINE def #-}

instance Monoid Progress where
  mempty  = def
  {-# INLINE mempty #-}

  mappend (Progress da la ua) (Progress db lb ub) = Progress
    { _downloaded = da + db
    , _left       = la + lb
    , _uploaded   = ua + ub
    }
  {-# INLINE mappend #-}

-- | Initial progress is used when there are no session before.
--
-- Please note that tracker might penalize client some way if the do
-- not accumulate progress. If possible and save 'Progress' between
-- client sessions to avoid that.
--
startProgress :: Integer -> Progress
startProgress = Progress 0 0 . fromIntegral
{-# INLINE startProgress #-}

-- | Used when the client download some data from /any/ peer.
downloadedProgress :: Int -> Progress -> Progress
downloadedProgress (fromIntegral -> amount)
                 = (left         -~ amount)
                 . (downloaded   +~ amount)
{-# INLINE downloadedProgress #-}

-- | Used when the client upload some data to /any/ peer.
uploadedProgress :: Int -> Progress -> Progress
uploadedProgress (fromIntegral -> amount) = uploaded +~ amount
{-# INLINE uploadedProgress #-}

-- | Used when leecher join client session.
enqueuedProgress :: Integer -> Progress -> Progress
enqueuedProgress amount = left +~ fromIntegral amount
{-# INLINE enqueuedProgress #-}

-- | Used when leecher leave client session.
--   (e.g. user deletes not completed torrent)
dequeuedProgress :: Integer -> Progress -> Progress
dequeuedProgress amount = left -~ fromIntegral amount
{-# INLINE dequeuedProgress #-}
