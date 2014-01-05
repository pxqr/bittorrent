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

         -- ** Construction
       , nullMagnet
       , simpleMagnet
       , detailedMagnet

         -- ** Conversion
       , parseMagnet
       , renderMagnet

         -- * URN
       , URN (..)

         -- ** Namespaces
       , NamespaceId
       , btih

         -- ** Construction
       , infohashURN

         -- ** Conversion
       , parseURN
       , renderURN
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


-- | Namespace identifier determines the syntactic interpretation of
-- namespace-specific string.
type NamespaceId = [Text]

-- | BitTorrent Info Hash (hence the name) namespace
-- identifier. Namespace-specific string /should/ be a base16\/base32
-- encoded SHA1 hash of the corresponding torrent /info/ dictionary.
--
btih :: NamespaceId
btih  = ["btih"]

-- | URN is pesistent location-independent identifier for
--   resources. In particular, URNs are used represent torrent names
--   as a part of magnet link, see 'Data.Torrent.Magnet.Magnet' for
--   more info.
--
data URN = URN
  { urnNamespace :: NamespaceId -- ^ a namespace identifier;
  , urnString    :: Text        -- ^ a corresponding
                                -- namespace-specific string.
  } deriving (Eq, Ord, Typeable)

{-----------------------------------------------------------------------
--  URN to infohash convertion
-----------------------------------------------------------------------}

instance Convertible URN InfoHash where
  safeConvert u @ URN {..}
    | urnNamespace /= btih = convError "invalid namespace" u
    |       otherwise      = safeConvert urnString

-- | Make resource name for torrent with corresponding
-- infohash. Infohash is base16 (hex) encoded.
--
infohashURN :: InfoHash -> URN
infohashURN = URN btih . longHex

-- | Meaningless placeholder value.
instance Default URN where
  def = infohashURN def

{-----------------------------------------------------------------------
--  URN Rendering
-----------------------------------------------------------------------}

-- | Render URN to its text representation.
renderURN :: URN -> Text
renderURN URN {..}
  = T.intercalate ":" $ "urn" : urnNamespace ++ [urnString]

instance Pretty URN where
  pretty = text . T.unpack . renderURN

instance Show URN where
  showsPrec n = showsPrec n . T.unpack . renderURN

instance QueryValueLike URN where
  toQueryValue = toQueryValue . renderURN
  {-# INLINE toQueryValue #-}

{-----------------------------------------------------------------------
--  URN Parsing
-----------------------------------------------------------------------}

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (L.init xs, L.last xs)

instance Convertible Text URN where
  safeConvert t = case T.split (== ':') t of
    uriScheme : body
      | T.toLower uriScheme == "urn" ->
        case unsnoc body of
          Just (namespace, val) -> pure URN
            { urnNamespace = namespace
            , urnString    = val
            }
          Nothing -> convError "missing URN string" body
      | otherwise -> convError "invalid URN scheme" uriScheme
    []            -> convError "missing URN scheme" t

instance IsString URN where
  fromString = either (error . prettyConvertError) id
             . safeConvert . T.pack

-- | Try to parse an URN from its text representation.
--
--  Use 'safeConvert' for detailed error messages.
--
parseURN :: Text -> Maybe URN
parseURN = either (const Nothing) pure . safeConvert

{-----------------------------------------------------------------------
--  Magnet
-----------------------------------------------------------------------}

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
--  Magnet Construction
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
    , tracker     = tAnnounce
    }

{-----------------------------------------------------------------------
--  Magnet Conversion
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
