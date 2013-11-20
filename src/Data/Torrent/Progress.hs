{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module Data.Torrent.Progress
       ( -- * Peer progress
         Progress (..)
       , left
       , uploaded
       , downloaded

       , startProgress

       , downloadedProgress
       , enqueuedProgress
       , uploadedProgress
       , dequeuedProgress

       ) where

import Control.Applicative
import Control.Lens
import Data.Aeson.TH
import Data.List as L
import Data.Default
import Data.Serialize as S


-- TODO: Use Word64?
-- TODO: Use atomic bits?

-- | 'Progress' contains upload/download/left stats about
--   current client state and used to notify the tracker.
--
-- Progress data is considered as dynamic within one client
-- session. This data also should be shared across client application
-- sessions (e.g. files), otherwise use 'startProgress' to get initial
-- 'Progress'.
--
data Progress = Progress
  { _downloaded :: !Integer -- ^ Total amount of bytes downloaded;
  , _left       :: !Integer -- ^ Total amount of bytes left;
  , _uploaded   :: !Integer -- ^ Total amount of bytes uploaded.
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

-- TODO Monoid instance

-- | Initial progress is used when there are no session before.
--
-- Please note that tracker might penalize client some way if the do
-- not accumulate progress. If possible and save 'Progress' between
-- client sessions to avoid that.
--
startProgress :: Integer -> Progress
startProgress = Progress 0 0
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
enqueuedProgress amount = left +~ amount
{-# INLINE enqueuedProgress #-}

-- | Used when leecher leave client session.
--   (e.g. user deletes not completed torrent)
dequeuedProgress :: Integer -> Progress -> Progress
dequeuedProgress amount = left -~ amount
{-# INLINE dequeuedProgress #-}
