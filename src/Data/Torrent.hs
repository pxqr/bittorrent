-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module provides torrent metainfo serialization.
module Data.Torrent
       ( module Data.Torrent.InfoHash
       , Torrent(..), ContentInfo(..), FileInfo(..)
       , contentLength, pieceCount, blockCount
       , Layout, contentLayout
       , isSingleFile, isMultiFile
       , fromFile
       ) where

import Control.Applicative
import Control.Arrow
import qualified Data.Map as M
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import           Data.Text (Text)
import Data.BEncode
import Data.Torrent.InfoHash
import Network.URI
import System.FilePath

type Time = Text

-- TODO more convenient form of torrent info.
-- | Metainfo about particular torrent.
data Torrent = Torrent {
      tInfoHash     :: InfoHash
      -- ^ SHA1 hash of the 'TorrentInfo' of the 'Torrent'.

    , tAnnounce     ::       URI
      -- ^ The URL of the tracker.

    , tAnnounceList :: Maybe [[URI]]
      -- ^ Announce list add multiple tracker support.
      --
      --   BEP 12: <http://www.bittorrent.org/beps/bep_0012.html>

    , tComment      :: Maybe Text
      -- ^ Free-form comments of the author.

    , tCreatedBy    :: Maybe ByteString
      -- ^ Name and version of the program used to create the .torrent.

    , tCreationDate :: Maybe Time
      -- ^ Creation time of the torrent, in standard UNIX epoch.

    , tEncoding     :: Maybe ByteString
      -- ^ String encoding format used to generate the pieces part of
      --   the info dictionary in the .torrent metafile.

    , tInfo         :: ContentInfo
      -- ^ Info about each content file.

    , tPublisher    :: Maybe URI
      -- ^ Containing the RSA public key of the publisher of the torrent.
      --   Private counterpart of this key that has the authority to allow
      --   new peers onto the swarm.

    , tPublisherURL :: Maybe URI
    , tSignature    :: Maybe ByteString
      -- ^ The RSA signature of the info dictionary (specifically,
      --   the encrypted SHA-1 hash of the info dictionary).
    } deriving Show

-- | Info part of the .torrent file contain info about each content file.
data ContentInfo =
    SingleFile {
      ciLength       :: Integer
      -- ^ Length of the file in bytes.

    , ciMD5sum       :: Maybe ByteString
      -- ^ 32 character long MD5 sum of the file.
      --   Used by third-party tools, not by bittorrent protocol itself.

    , ciName         :: ByteString
      -- ^ Suggested name of the file single file.



    , ciPieceLength  :: Int
      -- ^ Number of bytes in each piece.

    , ciPieces       :: ByteString
      -- ^ Concatenation of all 20-byte SHA1 hash values.

    , ciPrivate      :: Maybe Bool
      -- ^ If set the client MUST publish its presence to get other peers ONLY
      --   via the trackers explicity described in the metainfo file.
      --
      --   BEP 27: <http://www.bittorrent.org/beps/bep_0027.html>
    }

  | MultiFile {
      ciFiles        :: [FileInfo]
      -- ^ List of the all files that torrent contains.

    , ciName         :: ByteString
      -- | The file path of the directory in which to store all the files.

    , ciPieceLength  :: Int
    , ciPieces       :: ByteString
    , ciPrivate      :: Maybe Bool
    } deriving (Show, Read, Eq)


-- | Contain info about one single file.
data FileInfo = FileInfo {
      fiLength      :: Integer
      -- ^ Length of the file in bytes.

    , fiMD5sum      :: Maybe ByteString
      -- ^ 32 character long MD5 sum of the file.
      --   Used by third-party tools, not by bittorrent protocol itself.

    , fiPath        :: [ByteString]
      -- ^ One or more string elements that together represent the path and
      --   filename. Each element in the list corresponds to either a directory
      --   name or (in the case of the last element) the filename.
      --   For example, the file "dir1/dir2/file.ext" would consist of three
      --   string elements ["dir1", "dir2", "file.ext"]
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
    , "signature"     -->? tSignature t
    ]

  fromBEncode (BDict d) | Just info <- M.lookup "info" d =
    Torrent <$> pure (hashlazy (encode info)) -- WARN
            <*> d >--  "announce"
            <*> d >--? "announce-list"
            <*> d >--? "comment"
            <*> d >--? "created by"
            <*> d >--? "creation date"
            <*> d >--? "encoding"
            <*> d >--  "info"
            <*> d >--? "publisher"
            <*> d >--? "publisher-url"
            <*> d >--? "singature"

  fromBEncode _ = decodingError "Torrent"


instance BEncodable ContentInfo where
  toBEncode ti@(SingleFile { })  = fromAscAssocs
    [ "length"       -->  ciLength ti
    , "md5sum"       -->? ciMD5sum ti
    , "name"         -->  ciName   ti

    , "piece length" -->  ciPieceLength ti
    , "pieces"       -->  ciPieces  ti
    , "private"      -->? ciPrivate ti
    ]

  toBEncode ti@(MultiFile {}) = fromAscAssocs
    [ "files"        -->  ciFiles ti
    , "name"         -->  ciName  ti

    , "piece length" -->  ciPieceLength ti
    , "pieces"       -->  ciPieces  ti
    , "private"      -->? ciPrivate ti
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
  fromBEncode _ = decodingError "ContentInfo"


instance BEncodable FileInfo where
  toBEncode tf = fromAssocs
                 [ "length" -->  fiLength tf
                 , "md5sum" -->? fiMD5sum tf
                 , "path"   -->  fiPath tf
                 ]

  fromBEncode (BDict d) =
    FileInfo <$> d >--  "length"
                <*> d >--? "md5sum"
                <*> d >--  "path"

  fromBEncode _ = decodingError "FileInfo"

sizeInBase :: Integer -> Int -> Int
sizeInBase n b = fromIntegral (n `div` fromIntegral b) + align
  where
    align = if n `mod` fromIntegral b == 0 then 0 else 1

contentLength :: ContentInfo -> Integer
contentLength SingleFile { ciLength = len } = len
contentLength MultiFile  { ciFiles  = tfs } = sum (map fiLength tfs)

pieceCount :: ContentInfo -> Int
pieceCount ci = contentLength ci `sizeInBase` ciPieceLength ci

blockCount :: Int -> ContentInfo -> Int
blockCount blkSize ci = contentLength ci `sizeInBase` blkSize

-- | File layout specifies the order and the size of each file in the storage.
--   Note that order of files is highly important since we coalesce all
--   the files in the given order to get the linear block address space.
--
type Layout = [(FilePath, Int)]

contentLayout :: FilePath -> ContentInfo -> Layout
contentLayout rootPath = filesLayout
  where
    filesLayout   (SingleFile { ciName = name, ciLength = len })
      = [(rootPath </> BC.unpack name, fromIntegral len)]
    filesLayout   (MultiFile  { ciFiles = fs, ciName = dir }) =
      map (first mkPath . fl) fs
     where   -- TODO use utf8 encoding in name
        mkPath = ((rootPath </> BC.unpack dir) </>) . joinPath . map BC.unpack

    fl (FileInfo { fiPath = p, fiLength = len }) = (p, fromIntegral len)


isSingleFile :: ContentInfo -> Bool
isSingleFile SingleFile {} = True
isSingleFile _             = False

isMultiFile :: ContentInfo -> Bool
isMultiFile MultiFile {} = True
isMultiFile _            = False


-- | Read and decode a .torrent file.
fromFile :: FilePath -> IO (Result Torrent)
fromFile filepath = decoded <$> B.readFile filepath
