-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Torrent file contains metadata about files and folders but not
--   content itself. The files are bencoded dictionaries. There is
--   also other info which is used to help join the swarm.
--
--   This module provides torrent metainfo serialization and info hash
--   extraction.
--
--   For more info see:
--   <http://www.bittorrent.org/beps/bep_0003.html#metainfo-files>,
--   <https://wiki.theory.org/BitTorrentSpecification#Metainfo_File_Structure>
--
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -fno-warn-orphans           #-}
module Data.Torrent
       ( -- * Info dictionary
         InfoDict (..)
       , infoDictionary

         -- ** Lenses
       , infohash
       , layoutInfo
       , pieceInfo
       , isPrivate

         -- * Torrent file
       , Torrent(..)

         -- ** Lenses
       , announce
       , announceList
       , comment
       , createdBy
       , creationDate
       , encoding
       , infoDict
       , publisher
       , publisherURL
       , signature

         -- * Construction
       , nullTorrent

         -- * Mime types
       , typeTorrent

         -- * File paths
       , torrentExt
       , isTorrentPath

         -- * IO
       , fromFile
       , toFile
       ) where

import Prelude hiding (sum)
import Control.Applicative
import qualified Crypto.Hash.SHA1 as C
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Data.Aeson.Types (ToJSON(..), FromJSON(..), Value(..), withText)
import Data.Aeson.TH
import Data.BEncode as BE
import Data.BEncode.Types as BE
import           Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.ByteString.Lazy  as BL
import           Data.Char as Char
import           Data.Convertible
import           Data.Default
import           Data.Hashable   as Hashable
import qualified Data.List as L
import           Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import Network.URI
import Text.PrettyPrint as PP
import Text.PrettyPrint.Class
import System.FilePath

import Data.Torrent.InfoHash as IH
import Data.Torrent.Layout
import Data.Torrent.Piece


{-----------------------------------------------------------------------
--  Info dictionary
-----------------------------------------------------------------------}

{- note that info hash is actually reduntant field
   but it's better to keep it here to avoid heavy recomputations
-}

-- | Info part of the .torrent file contain info about each content file.
data InfoDict = InfoDict
  { idInfoHash     :: !InfoHash
    -- ^ SHA1 hash of the (other) 'DictInfo' fields.

  , idLayoutInfo   :: !LayoutInfo
    -- ^ File layout (name, size, etc) information.

  , idPieceInfo    :: !PieceInfo
    -- ^ Content validation information.

  , idPrivate      :: !Bool
    -- ^ If set the client MUST publish its presence to get other
    -- peers ONLY via the trackers explicity described in the
    -- metainfo file.
    --
    --   BEP 27: <http://www.bittorrent.org/beps/bep_0027.html>
  } deriving (Show, Read, Eq, Typeable)

$(deriveJSON defaultOptions { fieldLabelModifier =  (L.map Char.toLower . L.dropWhile isLower) } ''InfoDict)

makeLensesFor
  [ ("idInfoHash"  , "infohash"  )
  , ("idLayoutInfo", "layoutInfo")
  , ("idPieceInfo" , "pieceInfo" )
  , ("idPrivate"   , "isPrivate" )
  ]
  ''InfoDict

instance NFData InfoDict where
  rnf InfoDict {..} = rnf idLayoutInfo

instance Hashable InfoDict where
  hash = Hashable.hash . idInfoHash
  {-# INLINE hash #-}

-- | Smart constructor: add a info hash to info dictionary.
infoDictionary :: LayoutInfo -> PieceInfo -> Bool -> InfoDict
infoDictionary li pinfo private = InfoDict ih li pinfo private
  where
    ih = hashLazyIH $ encode $ InfoDict def li pinfo private

getPrivate :: Get Bool
getPrivate = (Just True ==) <$>? "private"

putPrivate :: Bool -> BDict -> BDict
putPrivate False = id
putPrivate True  = \ cont -> "private" .=! True .: cont

-- | Hash lazy bytestring using SHA1 algorithm.
hashLazyIH :: BL.ByteString -> InfoHash
hashLazyIH = either (const (error msg)) id . safeConvert . C.hashlazy
  where
    msg = "Infohash.hash: impossible: SHA1 is always 20 bytes long"

instance BEncode InfoDict where
  toBEncode InfoDict {..} = toDict $
      putLayoutInfo idLayoutInfo   $
      putPieceInfo  idPieceInfo    $
      putPrivate    idPrivate      $
      endDict

  fromBEncode dict = (`fromDict` dict) $ do
      InfoDict ih <$> getLayoutInfo
                  <*> getPieceInfo
                  <*> getPrivate
    where
      ih = hashLazyIH (encode dict)

ppPrivacy :: Bool -> Doc
ppPrivacy privacy = "Privacy: " <> if privacy then "private" else "public"

--ppAdditionalInfo :: InfoDict -> Doc
--ppAdditionalInfo layout = PP.empty

instance Pretty InfoDict where
  pretty InfoDict {..} =
    pretty idLayoutInfo $$
    pretty  idPieceInfo  $$
    ppPrivacy    idPrivate

{-----------------------------------------------------------------------
--  Torrent info
-----------------------------------------------------------------------}

-- | Metainfo about particular torrent.
data Torrent = Torrent
  { tAnnounce     :: !URI
    -- ^ The URL of the tracker.

  , tAnnounceList :: !(Maybe [[URI]])
    -- ^ Announce list add multiple tracker support.
    --
    --   BEP 12: <http://www.bittorrent.org/beps/bep_0012.html>

  , tComment      :: !(Maybe Text)
    -- ^ Free-form comments of the author.

  , tCreatedBy    :: !(Maybe Text)
    -- ^ Name and version of the program used to create the .torrent.

  , tCreationDate :: !(Maybe POSIXTime)
    -- ^ Creation time of the torrent, in standard UNIX epoch.

  , tEncoding     :: !(Maybe Text)
    -- ^ String encoding format used to generate the pieces part of
    --   the info dictionary in the .torrent metafile.

  , tInfoDict     :: !InfoDict
    -- ^ Info about each content file.

  , tPublisher    :: !(Maybe URI)
    -- ^ Containing the RSA public key of the publisher of the
    -- torrent.  Private counterpart of this key that has the
    -- authority to allow new peers onto the swarm.

  , tPublisherURL :: !(Maybe URI)
  , tSignature    :: !(Maybe ByteString)
    -- ^ The RSA signature of the info dictionary (specifically, the
    --   encrypted SHA-1 hash of the info dictionary).
    } deriving (Show, Eq, Typeable)

instance FromJSON URI where
  parseJSON = withText "URI" $
    maybe (fail "could not parse URI") pure . parseURI . T.unpack

instance ToJSON URI where
  toJSON = String . T.pack . show

instance ToJSON NominalDiffTime where
  toJSON = toJSON . posixSecondsToUTCTime

instance FromJSON NominalDiffTime where
  parseJSON v = utcTimeToPOSIXSeconds <$> parseJSON v

$(deriveJSON defaultOptions { fieldLabelModifier =  (L.map Char.toLower . L.dropWhile isLower) } ''Torrent)

makeLensesFor
  [ ("tAnnounce"    , "announce"    )
  , ("tAnnounceList", "announceList")
  , ("tComment"     , "comment"     )
  , ("tCreatedBy"   , "createdBy"   )
  , ("tCreationDate", "creationDate")
  , ("tEncoding"    , "encoding"    )
  , ("tInfoDict"    , "infoDict"    )
  , ("tPublisher"   , "publisher"   )
  , ("tPublisherURL", "publisherURL")
  , ("tSignature"   , "signature"   )
  ]
  ''Torrent

instance NFData Torrent where
  rnf Torrent {..} = rnf tInfoDict

-- TODO move to bencoding
instance BEncode URI where
  toBEncode uri = toBEncode (BC.pack (uriToString id uri ""))
  {-# INLINE toBEncode #-}

  fromBEncode (BString s) | Just url <- parseURI (BC.unpack s) = return url
  fromBEncode b           = decodingError $ "url <" ++ show b ++ ">"
  {-# INLINE fromBEncode #-}

--pico2uni :: Pico -> Uni
--pico2uni = undefined

-- TODO move to bencoding
instance BEncode POSIXTime where
  toBEncode pt = toBEncode (floor pt :: Integer)
  fromBEncode (BInteger i) = return $ fromIntegral i
  fromBEncode _            = decodingError $ "POSIXTime"

instance BEncode Torrent where
  toBEncode Torrent {..} = toDict $
       "announce"      .=! tAnnounce
    .: "announce-list" .=? tAnnounceList
    .: "comment"       .=? tComment
    .: "created by"    .=? tCreatedBy
    .: "creation date" .=? tCreationDate
    .: "encoding"      .=? tEncoding
    .: "info"          .=! tInfoDict
    .: "publisher"     .=? tPublisher
    .: "publisher-url" .=? tPublisherURL
    .: "signature"     .=? tSignature
    .: endDict

  fromBEncode = fromDict $ do
    Torrent <$>! "announce"
            <*>? "announce-list"
            <*>? "comment"
            <*>? "created by"
            <*>? "creation date"
            <*>? "encoding"
            <*>! "info"
            <*>? "publisher"
            <*>? "publisher-url"
            <*>? "signature"

(<:>) :: Doc -> Doc -> Doc
name <:>   v       = name <> ":" <+> v

(<:>?) :: Doc -> Maybe Doc -> Doc
_    <:>?  Nothing = PP.empty
name <:>? (Just d) = name <:> d

instance Pretty Torrent where
  pretty Torrent {..} =
       "InfoHash: " <> pretty (idInfoHash tInfoDict)
    $$ hang "General" 4 generalInfo
    $$ hang "Tracker" 4 trackers
    $$ pretty tInfoDict
   where
    trackers = case tAnnounceList of
        Nothing  -> text (show tAnnounce)
        Just xxs -> vcat $ L.map ppTier $ L.zip [1..] xxs
      where
        ppTier (n, xs) = "Tier #" <> int n <:> vcat (L.map (text . show) xs)

    generalInfo =
        "Comment"       <:>? ((text . T.unpack) <$> tComment)      $$
        "Created by"    <:>? ((text . T.unpack) <$> tCreatedBy)    $$
        "Created on"    <:>? ((text . show . posixSecondsToUTCTime)
                               <$> tCreationDate) $$
        "Encoding"      <:>? ((text . T.unpack) <$> tEncoding)     $$
        "Publisher"     <:>? ((text . show) <$> tPublisher)    $$
        "Publisher URL" <:>? ((text . show) <$> tPublisherURL) $$
        "Signature"     <:>? ((text . show) <$> tSignature)

-- | A simple torrent contains only required fields.
nullTorrent :: URI -> InfoDict -> Torrent
nullTorrent ann info = Torrent
    ann  Nothing Nothing Nothing Nothing Nothing
    info Nothing Nothing Nothing

-- | Mime type of torrent files.
typeTorrent :: BS.ByteString
typeTorrent = "application/x-bittorrent"

-- | Extension usually used for torrent files.
torrentExt :: String
torrentExt = "torrent"

-- | Test if this path has proper extension.
isTorrentPath :: FilePath -> Bool
isTorrentPath filepath = takeExtension filepath == extSeparator : torrentExt

-- | Read and decode a .torrent file.
fromFile :: FilePath -> IO Torrent
fromFile filepath = do
  contents <- BS.readFile filepath
  case decode contents of
    Right !t -> return t
    Left msg -> throwIO $ userError $ msg ++ " while reading torrent file"

-- | Encode and write a .torrent file.
toFile :: FilePath -> Torrent -> IO ()
toFile filepath = BL.writeFile filepath . encode
