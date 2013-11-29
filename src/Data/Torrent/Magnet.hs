-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  provisional
--   Portability :  portable
--
--   Magnet URI scheme is an standard defining Magnet links. Magnet
--   links are refer to resources by hash, in particular magnet links
--   can refer to torrent using corresponding infohash. In this way,
--   magnet links can be used instead of torrent files.
--
--   This module provides bittorrent specific implementation of magnet
--   links.
--
--   For more info see:
--   <http://magnet-uri.sourceforge.net/magnet-draft-overview.txt>
--
--   Bittorrent specific info:
--   <http://www.bittorrent.org/beps/bep_0009.html>
--
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# OPTIONS -fno-warn-orphans      #-}
module Data.Torrent.Magnet
       ( -- * Magnet
         Magnet(..)

         -- * Construction
       , nullMagnet
       , simpleMagnet
       , detailedMagnet

         -- * Conversion
       , parseMagnet
       , renderMagnet
       ) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 as BC
import Data.Convertible
import Data.Default
import Data.Map as M
import Data.Maybe
import Data.List as L
import Data.String
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Read
import Data.Typeable
import Network.HTTP.Types.QueryLike
import Network.HTTP.Types.URI
import Network.URI
import Text.PrettyPrint as PP
import Text.PrettyPrint.Class

import Data.Torrent
import Data.Torrent.InfoHash
import Data.Torrent.Layout
import Data.Torrent.URN


-- TODO multiple exact topics
-- TODO render/parse supplement for URI/query

-- | An URI used to identify torrent.
data Magnet = Magnet
  { -- | Torrent infohash hash. Can be used in DHT queries if no
    -- 'tracker' provided.
    exactTopic  :: !InfoHash -- TODO InfoHash -> URN?

    -- | A filename for the file to download. Can be used to
    -- display name while waiting for metadata.
  , displayName :: Maybe Text

    -- | Size of the resource in bytes.
  , exactLength :: Maybe Integer

    -- | URI pointing to manifest, e.g. a list of further items.
  , manifest :: Maybe Text

    -- | Search string.
  , keywordTopic :: Maybe Text

    -- | A source to be queried after not being able to find and
    -- download the file in the bittorrent network in a defined
    -- amount of time.
  , acceptableSource :: Maybe URI

    -- | Direct link to the resource.
  , exactSource      :: Maybe URI

    -- | URI to the tracker.
  , tracker :: Maybe URI

    -- | Additional or experimental parameters.
  , supplement :: Map Text Text
  } deriving (Eq, Ord, Typeable)

instance QueryValueLike Integer where
  toQueryValue = toQueryValue . show

instance QueryValueLike URI where
  toQueryValue = toQueryValue . show

instance QueryLike Magnet where
  toQuery Magnet {..} =
    [ ("xt", toQueryValue $ infohashURN exactTopic)
    , ("dn", toQueryValue displayName)
    , ("xl", toQueryValue exactLength)
    , ("mt", toQueryValue manifest)
    , ("kt", toQueryValue keywordTopic)
    , ("as", toQueryValue acceptableSource)
    , ("xs", toQueryValue exactSource)
    , ("tr", toQueryValue tracker)
    ]

instance QueryValueLike Magnet where
  toQueryValue = toQueryValue . renderMagnet

instance Convertible QueryText Magnet where
  safeConvert xs = do
      urnStr   <- getTextMsg "xt" "exact topic not defined" xs
      infoHash <- convertVia (error "safeConvert" :: URN)  urnStr
      return Magnet
        { exactTopic       = infoHash
        , displayName      = getText "dn" xs
        , exactLength      = getText "xl" xs >>= getInt
        , manifest         = getText "mt" xs
        , keywordTopic     = getText "kt" xs
        , acceptableSource = getText "as" xs >>= getURI
        , exactSource      = getText "xs" xs >>= getURI
        , tracker          = getText "tr" xs >>= getURI
        , supplement       = M.empty
        }
    where
      getInt    = either (const Nothing) (Just . fst) . signed decimal
      getURI    = parseURI . T.unpack
      getText p = join . L.lookup p
      getTextMsg p msg ps = maybe (convError msg xs) pure $ getText p ps

magnetScheme :: URI
magnetScheme = URI
    { uriScheme    = "magnet:"
    , uriAuthority = Nothing
    , uriPath      = ""
    , uriQuery     = ""
    , uriFragment  = ""
    }

isMagnetURI :: URI -> Bool
isMagnetURI u = u { uriQuery = "" } == magnetScheme

-- | Can be used instead of 'parseMagnet'.
instance Convertible URI Magnet where
  safeConvert u @ URI {..}
    | not (isMagnetURI u) = convError "this is not a magnet link" u
    |      otherwise      = safeConvert $ parseQueryText $ BC.pack uriQuery

-- | Can be used instead of 'renderMagnet'.
instance Convertible Magnet URI where
  safeConvert m = pure $ magnetScheme
    { uriQuery = BC.unpack $ renderQuery True $ toQuery m }

instance Convertible String Magnet where
  safeConvert str
    | Just uri <- parseURI str = safeConvert uri
    |        otherwise         = convError "unable to parse uri" str

{-----------------------------------------------------------------------
--  Construction
-----------------------------------------------------------------------}

-- | Meaningless placeholder value.
instance Default Magnet where
  def = Magnet
    { exactTopic       = def
    , displayName      = Nothing
    , exactLength      = Nothing
    , manifest         = Nothing
    , keywordTopic     = Nothing
    , acceptableSource = Nothing
    , exactSource      = Nothing
    , tracker          = Nothing
    , supplement       = M.empty
    }

-- | Set 'exactTopic' ('xt' param) only, other params are empty.
nullMagnet :: InfoHash -> Magnet
nullMagnet u = Magnet
    { exactTopic   = u
    , displayName  = Nothing
    , exactLength  = Nothing
    , manifest     = Nothing
    , keywordTopic = Nothing
    , acceptableSource = Nothing
    , exactSource      = Nothing
    , tracker    = Nothing
    , supplement = M.empty
    }

-- | Like 'nullMagnet' but also include 'displayName' ('dn' param).
simpleMagnet :: Torrent -> Magnet
simpleMagnet Torrent {tInfoDict = InfoDict {..}}
  = (nullMagnet idInfoHash)
    { displayName = Just $ T.decodeUtf8 $ suggestedName idLayoutInfo
    }

-- | Like 'simpleMagnet' but also include 'exactLength' ('xl' param) and
-- 'tracker' ('tr' param).
--
detailedMagnet :: Torrent -> Magnet
detailedMagnet t @ Torrent {tInfoDict = InfoDict {..}, tAnnounce}
  = (simpleMagnet t)
    { exactLength = Just $ fromIntegral $ contentLength idLayoutInfo
    , tracker     = Just tAnnounce
    }

{-----------------------------------------------------------------------
--  Conversion
-----------------------------------------------------------------------}

parseMagnetStr :: String -> Maybe Magnet
parseMagnetStr = either (const Nothing) Just . safeConvert

renderMagnetStr :: Magnet -> String
renderMagnetStr = show . (convert :: Magnet -> URI)

instance Pretty Magnet where
  pretty = PP.text . renderMagnetStr

instance Show Magnet where
  show = renderMagnetStr
  {-# INLINE show #-}

instance Read Magnet where
  readsPrec _ xs
      | Just m <- parseMagnetStr mstr = [(m, rest)]
      | otherwise = []
    where
      (mstr, rest) = L.break (== ' ') xs

instance IsString Magnet where
  fromString str = fromMaybe (error msg) $ parseMagnetStr str
    where
      msg = "unable to parse magnet: " ++ str

-- | Try to parse magnet link from urlencoded string. Use
-- 'safeConvert' to find out error location.
--
parseMagnet :: Text -> Maybe Magnet
parseMagnet = parseMagnetStr . T.unpack
{-# INLINE parseMagnet #-}

-- | Render magnet link to urlencoded string
renderMagnet :: Magnet -> Text
renderMagnet = T.pack . renderMagnetStr
{-# INLINE renderMagnet #-}
