-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides torrent metainfo serialization.
--
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- TODO refine interface
module Data.Torrent
       ( -- * Torrent
         Torrent(..), ContentInfo(..), FileInfo(..)
       , contentLength, pieceCount, blockCount
       , fromFile

         -- * Files layout
       , Layout, contentLayout
       , isSingleFile, isMultiFile

         -- * Info hash
       , InfoHash, ppInfoHash
       , hash, hashlazy
       , addHashToURI

         -- * Extra
       , sizeInBase

         -- * Internal
       , InfoHash(..)
       ) where

import Prelude hiding (sum)

import Control.Applicative
import Control.Arrow
import Data.BEncode as BE
import Data.Char
import Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.List as L
import           Data.Text (Text)
import           Data.Serialize as S hiding (Result)
import           Text.PrettyPrint
import qualified Crypto.Hash.SHA1 as C
import Network.URI
import System.FilePath
import Numeric


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
  toBEncode Torrent {..} = fromAscAssocs
    [ "announce"      -->  tAnnounce
    , "announce-list" -->? tAnnounceList
    , "comment"       -->? tComment
    , "created by"    -->? tCreatedBy
    , "creation date" -->? tCreationDate
    , "encoding"      -->? tEncoding
    , "info"          -->  tInfo
    , "publisher"     -->? tPublisher
    , "publisher-url" -->? tPublisherURL
    , "signature"     -->? tSignature
    ]

  fromBEncode (BDict d) | Just info <- M.lookup "info" d =
    Torrent <$> pure (hashlazy (BE.encode info)) -- WARN
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
  toBEncode SingleFile {..}  = fromAscAssocs
    [ "length"       -->  ciLength
    , "md5sum"       -->? ciMD5sum
    , "name"         -->  ciName

    , "piece length" -->  ciPieceLength
    , "pieces"       -->  ciPieces
    , "private"      -->? ciPrivate
    ]

  toBEncode MultiFile {..} = fromAscAssocs
    [ "files"        -->  ciFiles
    , "name"         -->  ciName

    , "piece length" -->  ciPieceLength
    , "pieces"       -->  ciPieces
    , "private"      -->? ciPrivate
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
  toBEncode FileInfo {..} = fromAssocs
                 [ "length" -->  fiLength
                 , "md5sum" -->? fiMD5sum
                 , "path"   -->  fiPath
                 ]

  fromBEncode (BDict d) =
    FileInfo <$> d >--  "length"
                <*> d >--? "md5sum"
                <*> d >--  "path"

  fromBEncode _ = decodingError "FileInfo"

sizeInBase :: Integral a => a -> Int -> Int
sizeInBase n b = fromIntegral (n `div` fromIntegral b) + align
  where
    align = if n `mod` fromIntegral b == 0 then 0 else 1
{-# SPECIALIZE sizeInBase :: Int -> Int -> Int #-}
{-# SPECIALIZE sizeInBase :: Integer -> Int -> Int #-}

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

{-----------------------------------------------------------------------
    Serialization
-----------------------------------------------------------------------}

-- | Exactly 20 bytes long SHA1 hash.
newtype InfoHash = InfoHash { getInfoHash :: ByteString }
                   deriving (Eq, Ord)

instance BEncodable InfoHash where
  toBEncode = toBEncode . getInfoHash
  fromBEncode be = InfoHash <$> fromBEncode be

instance Show InfoHash where
  show = render . ppInfoHash

instance Serialize InfoHash where
  put = putByteString . getInfoHash
  get = InfoHash <$> getBytes 20

instance BEncodable a => BEncodable (Map InfoHash a) where
  {-# SPECIALIZE instance BEncodable a => BEncodable (Map InfoHash a)  #-}
  fromBEncode b = M.mapKeys InfoHash <$> fromBEncode b
  {-# INLINE fromBEncode #-}

  toBEncode = toBEncode . M.mapKeys getInfoHash
  {-# INLINE toBEncode #-}

hash :: ByteString -> InfoHash
hash = InfoHash . C.hash

hashlazy :: Lazy.ByteString -> InfoHash
hashlazy = InfoHash . C.hashlazy

ppInfoHash :: InfoHash -> Doc
ppInfoHash = text . BC.unpack . Lazy.toStrict . ppHex . getInfoHash

ppHex :: ByteString -> Lazy.ByteString
ppHex = B.toLazyByteString . foldMap (B.primFixed B.word8HexFixed) . B.unpack

addHashToURI :: URI -> InfoHash -> URI
addHashToURI uri s = uri {
    uriQuery = uriQuery uri ++ mkPref (uriQuery uri) ++
               "info_hash=" ++ rfc1738Encode (BC.unpack (getInfoHash s))
  }
  where
    mkPref [] = "?"
    mkPref ('?' : _) = "&"
    mkPref _ = error "addHashToURI"

    rfc1738Encode = L.concatMap (\c -> if unreservedS c then [c] else encodeHex c)
      where
        unreservedS = (`L.elem` chars)
        chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_./"
        encodeHex c = '%' : pHex c
        pHex c = let p = (showHex . ord $ c) ""
                 in if L.length p == 1 then '0' : p else p
