-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  provisional
--   Portability :  portable
--
--   URN is pesistent location-independent identifier for
--   resources. In particular, URNs are used represent torrent names
--   as a part of magnet link, see 'Data.Torrent.Magnet.Magnet' for
--   more info.
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module Data.Torrent.URN
       ( -- * Namespaces
         NamespaceId
       , btih

         -- * URN
       , URN (..)

         -- ** Construction
       , infohashURN

         -- ** Conversion
       , parseURN
       , renderURN
       ) where

import Control.Applicative
import Data.Convertible
import Data.Default
import Data.List as L
import Data.String
import Data.Text as T
import Data.Typeable
import Network.HTTP.Types.QueryLike
import Text.PrettyPrint
import Text.PrettyPrint.Class

import Data.Torrent.InfoHash


-- | Namespace identifier determines the syntactic interpretation of
-- namespace-specific string.
type NamespaceId = [Text]

-- | BitTorrent Info Hash (hence the name) namespace
-- identifier. Namespace-specific string /should/ be a base16\/base32
-- encoded SHA1 hash of the corresponding torrent /info/ dictionary.
--
btih :: NamespaceId
btih  = ["btih"]

-- | Uniform Resource Name - location-independent, resource
-- identifier.
--
data URN = URN
  { urnNamespace :: NamespaceId -- ^ a namespace identifier;
  , urnString    :: Text        -- ^ a corresponding
                                -- namespace-specific string.
  } deriving (Eq, Ord, Typeable)

{-----------------------------------------------------------------------
--  Infohash convertion
-----------------------------------------------------------------------}

instance Convertible URN InfoHash where
  safeConvert u @ URN {..}
    | urnNamespace /= btih = convError "invalid namespace" u
    |       otherwise      = safeConvert urnString

-- | Make resource name for torrent with corresponding
-- infohash. Infohash is base16 (hex) encoded).
--
infohashURN :: InfoHash -> URN
infohashURN = URN btih . longHex

-- | Meaningless placeholder value.
instance Default URN where
  def = infohashURN def

{-----------------------------------------------------------------------
--  Rendering
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
--  Parsing
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
