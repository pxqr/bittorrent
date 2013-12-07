module Data.Torrent.JSON
       ( omitLensPrefix
       , omitRecordPrefix
       ) where

import Data.Aeson.TH
import Data.Char
import Data.List as L


-- | Ignore '_' prefix.
omitLensPrefix :: Options
omitLensPrefix = defaultOptions
  { fieldLabelModifier     = L.dropWhile (== '_')
  , constructorTagModifier = id
  , allNullaryToStringTag  = True
  , omitNothingFields      = True
  }

mapWhile :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhile p f = go
  where
    go []       = []
    go (x : xs)
      |    p x    = f x : go xs
      | otherwise = xs

omitRecordPrefix :: Options
omitRecordPrefix = omitLensPrefix
  { fieldLabelModifier = mapWhile isUpper toLower . L.dropWhile isLower
  }