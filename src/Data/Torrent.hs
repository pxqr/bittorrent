{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module provides torrent metainfo serialization.
module Data.Torrent
       ( module Data.Torrent.InfoHash
       , Torrent(..), TorrentInfo(..), TorrentFile(..)
       , fromFile
       ) where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import           Data.Text (Text)
import Data.BEncode
import Data.Torrent.InfoHash
import Network.URI

type Time = Text

-- TODO comment fields
-- TODO more convenient form of torrent info.
data Torrent = Torrent {
      tInfoHash     :: InfoHash
    , tAnnounce     ::       URI
    , tAnnounceList :: Maybe [[URI]]
    , tComment      :: Maybe Text
    , tCreatedBy    :: Maybe ByteString
    , tCreationDate :: Maybe Time
    , tEncoding     :: Maybe ByteString
    , tInfo         :: TorrentInfo
    , tPublisher    :: Maybe URI
    , tPublisherURL :: Maybe URI
    } deriving Show

data TorrentInfo =
    SingleFile {
      tLength       :: Int
    , tMD5sum       :: Maybe ByteString
    , tName         :: ByteString

    , tPieceLength  :: Int
    , tPieces       :: ByteString -- Vector ByteString?
    , tPrivate      :: Maybe Bool
    }
  | MultiFile {
      tFiles        :: [TorrentFile]
    , tName         :: ByteString

    , tPieceLength  :: Int
    , tPieces       :: ByteString -- Vector ByteString?
    , tPrivate      :: Maybe Bool
    } deriving (Show, Read, Eq)

data TorrentFile = TorrentFile {
      tfLength      :: Int
    , tfMD5sum      :: Maybe ByteString
    , tfPath        :: [ByteString]
    } deriving (Show, Read, Eq)

instance BEncodable URI where
  toBEncode uri = toBEncode (BC.pack (uriToString id uri ""))
  {-# INLINE toBEncode #-}

  fromBEncode (BString s) | Just url <- parseURI (BC.unpack s) = return url
  fromBEncode b           = decodingError $ "url <" ++ show b ++ ">"
  {-# INLINE fromBEncode #-}

instance BEncodable Torrent where
  toBEncode t = fromAscAssocs
    [ "announce"      -->  tAnnounce t
    , "announce-list" -->? tAnnounceList t
    , "comment"       -->? tComment t
    , "created by"    -->? tCreatedBy t
    , "creation date" -->? tCreationDate t
    , "encoding"      -->? tEncoding t
    , "info"          -->  tInfo t
    , "publisher"     -->? tPublisher t
    , "publisher-url" -->? tPublisherURL t
    ]

  fromBEncode (BDict d) | Just info <- M.lookup "info" d =
    Torrent <$> pure (hashlazy (encode info))
            <*> d >--  "announce"
            <*> d >--? "announce-list"
            <*> d >--? "comment"
            <*> d >--? "created by"
            <*> d >--? "creation date"
            <*> d >--? "encoding"
            <*> d >--  "info"
            <*> d >--? "publisher"
            <*> d >--? "publisher-url"

  fromBEncode _ = decodingError "Torrent"


instance BEncodable TorrentInfo where
  toBEncode ti@(SingleFile { })  = fromAscAssocs
    [ "length"       -->  tLength ti
    , "md5sum"       -->? tMD5sum ti
    , "name"         -->  tName ti

    , "piece length" -->  tPieceLength ti
    , "pieces"       -->  tPieces ti
    , "private"      -->? tPrivate ti
    ]

  toBEncode ti@(MultiFile {}) = fromAscAssocs
    [ "files"        -->  tFiles ti
    , "name"         -->  tName ti

    , "piece length" -->  tPieceLength ti
    , "pieces"       -->  tPieces ti
    , "private"      -->? tPrivate ti
    ]

  fromBEncode (BDict d)
    | Just (BList fs) <- M.lookup "files" d =
      MultiFile   <$> mapM fromBEncode fs
                  <*> d >--  "name"
                  <*> d >--  "piece length"
                  <*> d >--  "pieces"
                  <*> d >--? "private"
    | otherwise =
      SingleFile  <$> d >--  "length"
                  <*> d >--? "md5sum"
                  <*> d >--  "name"
                  <*> d >--  "piece length"
                  <*> d >--  "pieces"
                  <*> d >--? "private"
  fromBEncode _ = decodingError "TorrentInfo"


instance BEncodable TorrentFile where
  toBEncode tf = fromAssocs
                 [ "length" -->  tfLength tf
                 , "md5sum" -->? tfMD5sum tf
                 , "path"   -->  tfPath tf
                 ]

  fromBEncode (BDict d) =
    TorrentFile <$> d >--  "length"
                <*> d >--? "md5sum"
                <*> d >--  "path"

  fromBEncode _ = decodingError "TorrentFile"


fromFile :: FilePath -> IO (Result Torrent)
fromFile filepath = (fromBEncode <=< decode) <$> B.readFile filepath
