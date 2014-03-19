{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Torrent.JSON
       ( omitLensPrefix
       , omitRecordPrefix
       ) where

import Control.Applicative
import Data.Aeson.TH
import Data.Aeson.Types
import Data.ByteString as BS
import Data.ByteString.Base16 as Base16
import Data.Char
import Data.IP
import Data.List as L
import Data.Text.Encoding as T


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

instance ToJSON ByteString where
  toJSON = String . T.decodeUtf8 . Base16.encode

instance FromJSON ByteString where
  parseJSON v = do
    (ok, bad) <- (Base16.decode . T.encodeUtf8) <$> parseJSON v
    if BS.null bad
      then return ok
      else fail   "parseJSON: unable to decode ByteString"

instance ToJSON IP where
  toJSON = toJSON . show

instance FromJSON IP where
  parseJSON v = do
    str <- parseJSON v
    return $ read str
