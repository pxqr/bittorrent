-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Parsing and rendering of magnet URIs.
--
--   For more info see:
--   <http://magnet-uri.sourceforge.net/magnet-draft-overview.txt>
--
--   Bittorrent specific info:
--   <http://www.bittorrent.org/beps/bep_0009.html>
--
{-# LANGUAGE NamedFieldPuns #-}
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

         -- ** Extra
       , fromURI
       , toURI
       ) where

import Control.Applicative
import Control.Monad
import Data.Map as M
import Data.Maybe
import Data.List as L
import Data.URLEncoded as URL
import Data.String
import Data.Text as T
import Data.Text.Encoding as T
import Network.URI
import Text.Read
import Text.PrettyPrint as PP
import Text.PrettyPrint.Class

import Data.Torrent
import Data.Torrent.InfoHash
import Data.Torrent.Layout

{-----------------------------------------------------------------------
-- URN
-----------------------------------------------------------------------}

type NamespaceId = [Text]

btih :: NamespaceId
btih  = ["btih"]

-- | Uniform Resource Name - location-independent, resource
-- identifier.
data URN = URN
    { urnNamespace :: NamespaceId
    , urnString    :: Text
    } deriving (Eq, Ord)

instance Show URN where
  showsPrec n = showsPrec n . T.unpack . renderURN

instance IsString URN where
  fromString = fromMaybe def . parseURN . T.pack
    where
      def = error "unable to parse URN"

instance URLShow URN where
  urlShow = T.unpack . renderURN

parseURN :: Text -> Maybe URN
parseURN str = case T.split (== ':') str of
    uriScheme : body
      | T.toLower uriScheme == "urn" -> mkURN body
      |          otherwise           -> Nothing
    [] -> Nothing
  where
    mkURN [] = Nothing
    mkURN xs = Just $ URN
        { urnNamespace = L.init xs
        , urnString    = L.last xs
        }

renderURN :: URN -> Text
renderURN URN {..}
  = T.intercalate ":" $ "urn" : urnNamespace ++ [urnString]

urnToInfoHash :: URN -> Maybe InfoHash
urnToInfoHash  (URN {..})
  | urnNamespace /= btih = Nothing
  |       otherwise      = textToInfoHash urnString

infoHashToURN :: InfoHash -> URN
infoHashToURN = URN btih . T.pack . show

{-----------------------------------------------------------------------
--  Magnet
-----------------------------------------------------------------------}

-- TODO multiple exact topics
-- TODO supplement

-- | An URI used to identify torrent.
data Magnet = Magnet
    { -- | Resource hash.
      exactTopic  :: !InfoHash
      -- | Might be used to display name while waiting for metadata.
    , displayName :: Maybe Text
      -- | Size of the resource in bytes.
    , exactLength :: Maybe Integer

    , manifest :: Maybe String
      -- | Search string.
    , keywordTopic :: Maybe String

    , acceptableSource :: Maybe URI
    , exactSource      :: Maybe URI

    , tracker :: Maybe URI

    , supplement :: Map Text Text
    } deriving (Eq, Ord)

instance Show Magnet where
  show = renderMagnet
  {-# INLINE show #-}

instance Read Magnet where
  readsPrec _ xs
      | Just m <- parseMagnet mstr = [(m, rest)]
      | otherwise = []
    where
      (mstr, rest) = L.break (== ' ') xs

instance IsString Magnet where
  fromString = fromMaybe def . parseMagnet
    where
      def = error "unable to parse magnet"

instance URLEncode Magnet where
  urlEncode = toQuery
  {-# INLINE urlEncode #-}

instance Pretty Magnet where
  pretty = PP.text . renderMagnet

-- | Set exact topic only, other params are empty.
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

-- | A simple magnet link including infohash ('xt' param) and display
-- name ('dn' param).
--
simpleMagnet :: Torrent -> Magnet
simpleMagnet Torrent {tInfoDict = InfoDict {..}}
  = (nullMagnet idInfoHash)
    { displayName = Just $ T.decodeUtf8 $ suggestedName idLayoutInfo
    }

-- | Like 'simpleMagnet' but also include exactLength ('xl' param) and
-- tracker ('tr' param).
detailedMagnet :: Torrent -> Magnet
detailedMagnet t @ Torrent {tInfoDict = InfoDict {..}, tAnnounce}
  = (simpleMagnet t)
    { exactLength = Just $ fromIntegral $ contentLength idLayoutInfo
    , tracker     = Just tAnnounce
    }

fromQuery :: URLEncoded -> Either String Magnet
fromQuery q
  | Just urnStr   <- URL.lookup ("xt" :: String) q
  , Just urn      <- parseURN $ T.pack urnStr
  , Just infoHash <- urnToInfoHash urn
  = return $ Magnet
      { exactTopic  = infoHash
      , displayName = T.pack    <$> URL.lookup ("dn" :: String) q
      , exactLength = readMaybe =<< URL.lookup ("xl" :: String) q

      , manifest     = URL.lookup ("mt" :: String) q
      , keywordTopic = URL.lookup ("kt" :: String) q

      , acceptableSource = parseURI =<< URL.lookup ("as" :: String) q
      , exactSource      = parseURI =<< URL.lookup ("xs" :: String) q

      , tracker    = parseURI =<< URL.lookup ("tr" :: String) q
      , supplement = M.empty
      }

  | otherwise = Left "exact topic not defined"

toQuery :: Magnet -> URLEncoded
toQuery Magnet {..}
    =  s "xt" %=  infoHashToURN exactTopic
    %& s "dn" %=? (T.unpack <$> displayName)
    %& s "xl" %=? exactLength
    %& s "mt" %=? manifest
    %& s "kt" %=? keywordTopic
    %& s "as" %=? acceptableSource
    %& s "xs" %=? exactSource
    %& s "tr" %=? tracker
  where
    s :: String -> String; s = id

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

-- | The same as 'parseMagnet' but useful if you alread have a parsed
-- uri.
fromURI :: URI -> Either String Magnet
fromURI u @ URI {..}
  | not (isMagnetURI u) = Left "this is not a magnet link"
  |      otherwise      = importURI u >>= fromQuery

-- | The same as 'renderMagnet' but useful if you need an uri.
toURI :: Magnet -> URI
toURI m = magnetScheme %? urlEncode m

etom :: Either a b -> Maybe b
etom = either (const Nothing) Just

-- | Try to parse magnet link from urlencoded string.
parseMagnet :: String -> Maybe Magnet
parseMagnet = parseURI >=> etom . fromURI

-- | Render magnet link to urlencoded string
renderMagnet :: Magnet -> String
renderMagnet = show . toURI
