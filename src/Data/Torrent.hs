-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
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
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE TemplateHaskell   #-}
-- TODO refine interface
module Data.Torrent
       ( -- * Torrent
         Torrent(..), ContentInfo(..), FileInfo(..)
       , mktorrent, simpleTorrent
       , torrentExt, isTorrentPath
       , fromFile

         -- * Files layout
       , Layout, contentLayout
       , contentLength, fileOffset
       , pieceCount, blockCount
       , isSingleFile, isMultiFile

       , checkPiece

         -- * Info hash
#if defined (TESTING)
       , InfoHash(..)
#else
       , InfoHash(..)
#endif
       , ppInfoHash
       , addHashToURI

         -- * Extra
       , sizeInBase

#if defined (TESTING)
         -- * Internal
       , Data.Torrent.hash
       , Data.Torrent.hashlazy
       , layoutOffsets
#endif
       ) where

import Prelude hiding (sum)

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad

import qualified Crypto.Hash.SHA1 as C

import Data.Aeson.TH
import Data.BEncode as BE
import Data.Char
import Data.Foldable
import qualified Data.ByteString as B
import           Data.ByteString.Internal
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy.Builder.ASCII as B
import qualified Data.List as L
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Hashable as Hashable
import           Data.Text (Text)
import           Data.Serialize as S hiding (Result)
import           Text.PrettyPrint
import           Text.ParserCombinators.ReadP as P
import           Text.Read

import Network.URI
import System.FilePath
import Numeric

{-----------------------------------------------------------------------
    Info hash
-----------------------------------------------------------------------}

-- | Exactly 20 bytes long SHA1 hash of the info part of torrent file.
newtype InfoHash = InfoHash { getInfoHash :: ByteString }
                   deriving (Eq, Ord)

instance Show InfoHash where
  show = render . ppInfoHash

instance Read InfoHash where
  readsPrec _ = readP_to_S $ do
      str <- replicateM 40 (satisfy isHexDigit)
      return $ InfoHash $ decodeIH str
    where
      decodeIH       = B.pack . map fromHex . pair
      fromHex (a, b) = read $ '0' : 'x' : a : b : []

      pair (a : b : xs) = (a, b) : pair xs
      pair _            = []

instance Hashable InfoHash where
  hash = Hashable.hash . getInfoHash

instance BEncodable InfoHash where
  toBEncode = toBEncode . getInfoHash
  fromBEncode be = InfoHash <$> fromBEncode be

instance Serialize InfoHash where
  put = putByteString . getInfoHash
  get = InfoHash <$> getBytes 20

instance BEncodable a => BEncodable (Map InfoHash a) where
  {-# SPECIALIZE instance BEncodable a => BEncodable (Map InfoHash a)  #-}
  fromBEncode b = M.mapKeys InfoHash <$> fromBEncode b
  {-# INLINE fromBEncode #-}

  toBEncode = toBEncode . M.mapKeys getInfoHash
  {-# INLINE toBEncode #-}

-- | Hash strict bytestring using SHA1 algorithm.
hash :: ByteString -> InfoHash
hash = InfoHash . C.hash

-- | Hash lazy bytestring using SHA1 algorithm.
hashlazy :: Lazy.ByteString -> InfoHash
hashlazy = InfoHash . C.hashlazy

-- | Pretty print info hash in hexadecimal format.
ppInfoHash :: InfoHash -> Doc
ppInfoHash = text . BC.unpack . ppHex . getInfoHash

ppHex :: ByteString -> ByteString
ppHex = Lazy.toStrict . B.toLazyByteString . B.byteStringHexFixed

-- | Add query info hash parameter to uri.
--
--   > info_hash=<url_encoded_info_hash>
--
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

{-----------------------------------------------------------------------
    Torrent metainfo
-----------------------------------------------------------------------}

type Time = Text

-- | Contain info about one single file.
data FileInfo = FileInfo {
      fiLength      :: !Integer
      -- ^ Length of the file in bytes.

    , fiMD5sum      ::  Maybe ByteString
      -- ^ 32 character long MD5 sum of the file.
      --   Used by third-party tools, not by bittorrent protocol itself.

    , fiPath        :: ![ByteString]
      -- ^ One or more string elements that together represent the
      --   path and filename. Each element in the list corresponds to
      --   either a directory name or (in the case of the last
      --   element) the filename.  For example, the file:
      --
      --   > "dir1/dir2/file.ext"
      --
      --   would consist of three string elements:
      --
      --   > ["dir1", "dir2", "file.ext"]
      --
    } deriving (Show, Read, Eq)

$(deriveJSON (L.map toLower . L.dropWhile isLower) ''FileInfo)


-- | Info part of the .torrent file contain info about each content file.
data ContentInfo =
    SingleFile {
      ciLength       :: !Integer
      -- ^ Length of the file in bytes.

    , ciMD5sum       :: Maybe ByteString
      -- ^ 32 character long MD5 sum of the file.
      --   Used by third-party tools, not by bittorrent protocol itself.

    , ciName         :: !ByteString
      -- ^ Suggested name of the file single file.



    , ciPieceLength  :: !Int
      -- ^ Number of bytes in each piece.

    , ciPieces       :: !ByteString
      -- ^ Concatenation of all 20-byte SHA1 hash values.

    , ciPrivate      :: Maybe Bool
      -- ^ If set the client MUST publish its presence to get other
      -- peers ONLY via the trackers explicity described in the
      -- metainfo file.
      --
      --   BEP 27: <http://www.bittorrent.org/beps/bep_0027.html>
    }

  | MultiFile {
      ciFiles        :: ![FileInfo]
      -- ^ List of the all files that torrent contains.

    , ciName         :: !ByteString
      -- | The file path of the directory in which to store all the files.

    , ciPieceLength  :: !Int
    , ciPieces       :: !ByteString
    , ciPrivate      :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON id ''ContentInfo)

-- TODO more convenient form of torrent info.
-- | Metainfo about particular torrent.
data Torrent = Torrent {
      tInfoHash     :: !InfoHash
      -- ^ SHA1 hash of the 'TorrentInfo' of the 'Torrent'.

    , tAnnounce     :: !URI
      -- ^ The URL of the tracker.

      -- NOTE: out of lexicographic order!
    , tInfo         :: !ContentInfo
      -- ^ Info about each content file.

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

    , tPublisher    :: Maybe URI
      -- ^ Containing the RSA public key of the publisher of the
      -- torrent.  Private counterpart of this key that has the
      -- authority to allow new peers onto the swarm.

    , tPublisherURL :: Maybe URI
    , tSignature    :: Maybe ByteString
      -- ^ The RSA signature of the info dictionary (specifically,
      --   the encrypted SHA-1 hash of the info dictionary).
    } deriving (Show, Eq)

instance Hashable Torrent where
  hash = Hashable.hash . tInfoHash

{- note that info hash is actually reduntant field
   but it's better to keep it here to avoid heavy recomputations
-}

-- | Smart constructor for 'Torrent' which compute info hash.
mktorrent :: URI            -> ContentInfo
        -> Maybe [[URI]]  -> Maybe Text       -> Maybe ByteString
        -> Maybe Time     -> Maybe ByteString -> Maybe URI
        -> Maybe URI      -> Maybe ByteString
        -> Torrent
mktorrent announce info = Torrent (hashlazy (BE.encoded info)) announce info

-- | A simple torrent contains only required fields.
simpleTorrent :: URI -> ContentInfo -> Torrent
simpleTorrent announce info = mktorrent announce info
                              Nothing Nothing Nothing
                              Nothing Nothing Nothing
                              Nothing Nothing

-- TODO check if pieceLength is power of 2

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
            <*> d >--  "info"
            <*> d >--? "announce-list"
            <*> d >--? "comment"
            <*> d >--? "created by"
            <*> d >--? "creation date"
            <*> d >--? "encoding"
            <*> d >--? "publisher"
            <*> d >--? "publisher-url"
            <*> d >--? "signature"

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


-- | Divide and round up.
sizeInBase :: Integral a => a -> Int -> Int
sizeInBase n b = fromIntegral (n `div` fromIntegral b) + align
  where
    align = if n `mod` fromIntegral b == 0 then 0 else 1
{-# SPECIALIZE sizeInBase :: Int -> Int -> Int #-}
{-# SPECIALIZE sizeInBase :: Integer -> Int -> Int #-}


-- | Find sum of sizes of the all torrent files.
contentLength :: ContentInfo -> Integer
contentLength SingleFile { ciLength = len } = len
contentLength MultiFile  { ciFiles  = tfs } = sum (map fiLength tfs)

-- | Find count of pieces in the torrent. If torrent size is not a
-- multiple of piece size then the count is rounded up.
pieceCount :: ContentInfo -> Int
pieceCount ci = contentLength ci `sizeInBase` ciPieceLength ci

-- | Find number of blocks of the specified size. If torrent size is
-- not a multiple of block size then the count is rounded up.
blockCount :: Int         -- ^ Block size.
           -> ContentInfo -- ^ Torrent content info.
           -> Int         -- ^ Number of blocks.
blockCount blkSize ci = contentLength ci `sizeInBase` blkSize

-- | File layout specifies the order and the size of each file in the
--   storage. Note that order of files is highly important since we
--   coalesce all the files in the given order to get the linear block
--   address space.
--
type Layout = [(FilePath, Int)]

-- | Extract files layout from torrent info with the given root path.
contentLayout :: FilePath    -- ^ Root path for the all torrent files.
              -> ContentInfo -- ^ Torrent content information.
              -> Layout      -- ^ The all file paths prefixed with the
                             -- given root.
contentLayout rootPath = filesLayout
  where
    filesLayout   (SingleFile { ciName = name, ciLength = len })
      = [(rootPath </> BC.unpack name, fromIntegral len)]
    filesLayout   (MultiFile  { ciFiles = fs, ciName = dir }) =
      map (first mkPath . fl) fs
     where   -- TODO use utf8 encoding in name
        mkPath = ((rootPath </> BC.unpack dir) </>) . joinPath . map BC.unpack

    fl (FileInfo { fiPath = p, fiLength = len }) = (p, fromIntegral len)

layoutOffsets :: Layout -> Layout
layoutOffsets = go 0
  where
    go !_ [] = []
    go !offset ((n, s) : xs) = (n, offset) : go (offset + s) xs

-- | Gives global offset of a content file for a given full path.
fileOffset :: FilePath -> ContentInfo -> Maybe Integer
fileOffset fullPath
  = fmap fromIntegral . lookup fullPath . layoutOffsets . contentLayout ""

-- | Test if this is single file torrent.
isSingleFile :: ContentInfo -> Bool
isSingleFile SingleFile {} = True
isSingleFile _             = False

-- | Test if this is multifile torrent.
isMultiFile :: ContentInfo -> Bool
isMultiFile MultiFile {} = True
isMultiFile _            = False

slice :: Int -> Int -> ByteString -> ByteString
slice from to = B.take to . B.drop from

-- | Extract validation hash by specified piece index.
pieceHash :: ContentInfo -> Int -> ByteString
pieceHash ci ix = slice (hashsize * ix) hashsize (ciPieces ci)
  where
    hashsize   = 20

-- | Validate piece with metainfo hash.
checkPiece :: ContentInfo -> Int -> ByteString -> Bool
checkPiece ci ix piece
  =  B.length piece == ciPieceLength ci
  && C.hash   piece == pieceHash ci ix

-- | Extension usually used for torrent metafiles.
torrentExt :: String
torrentExt = "torrent"

-- | Test if this path has proper extension.
isTorrentPath :: FilePath -> Bool
isTorrentPath filepath = takeExtension filepath == extSeparator : torrentExt

-- | Read and decode a .torrent file.
fromFile :: FilePath -> IO Torrent
fromFile filepath = do
  contents <- B.readFile filepath
  case decoded contents of
    Right !t -> return t
    Left msg -> throwIO $ userError  $ msg ++ " while reading torrent"