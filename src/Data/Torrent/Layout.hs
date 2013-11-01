-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Layout of files in torrent.
--
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS  -fno-warn-orphans          #-}
module Data.Torrent.Layout
       ( -- * File attributes
         FileOffset
       , FileSize

         -- * Single file info
       , FileInfo (..)

         -- ** Lens
       , fileLength
       , filePath
       , fileMD5Sum

         -- * File layout
       , LayoutInfo (..)

         -- ** Lens
       , singleFile
       , multiFile
       , rootDirName

         -- ** Predicates
       , isSingleFile
       , isMultiFile

         -- ** Folds
       , contentLength
       , fileCount
       , blockCount

         -- * Flat file layout
       , Layout
       , flatLayout
       , accumOffsets
       , fileOffset

         -- * Internal
       , getLayoutInfo
       , putLayoutInfo
       ) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Data.Aeson.TH
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.BEncode
import Data.BEncode.Types
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.Char
import Data.List as L
import Data.Typeable
import System.FilePath
import System.Posix.Types

import Data.Torrent.Block


{-----------------------------------------------------------------------
--  File attribytes
-----------------------------------------------------------------------}

-- | Size of a file in bytes.
type FileSize = FileOffset

deriving instance FromJSON FileOffset
deriving instance ToJSON   FileOffset
deriving instance BEncode  FileOffset

{-----------------------------------------------------------------------
--  File info both either from info dict or file list
-----------------------------------------------------------------------}

-- | Contain metainfo about one single file.
data FileInfo a = FileInfo {
      fiLength      :: {-# UNPACK #-} !FileSize
      -- ^ Length of the file in bytes.

      -- TODO unpacked MD5 sum
    , fiMD5Sum      :: !(Maybe ByteString)
      -- ^ 32 character long MD5 sum of the file.  Used by third-party
      -- tools, not by bittorrent protocol itself.

    , fiName        :: !a
      -- ^ One or more string elements that together represent the
      -- path and filename. Each element in the list corresponds to
      -- either a directory name or (in the case of the last element)
      -- the filename.  For example, the file:
      --
      --   > "dir1/dir2/file.ext"
      --
      --   would consist of three string elements:
      --
      --   > ["dir1", "dir2", "file.ext"]
      --
    } deriving (Show, Read, Eq, Typeable)

$(deriveJSON (L.map toLower . L.dropWhile isLower) ''FileInfo)

makeLensesFor
  [ ("fiLength", "fileLength")
  , ("fiMD5Sum", "fileMD5Sum")
  , ("fiName"  , "filePath"  )
  ]
  ''FileInfo

instance NFData a => NFData (FileInfo a) where
  rnf FileInfo {..} = rnf fiName
  {-# INLINE rnf #-}

instance BEncode (FileInfo [ByteString]) where
  toBEncode FileInfo {..} = toDict $
       "length" .=! fiLength
    .: "md5sum" .=? fiMD5Sum
    .: "path"   .=! fiName
    .: endDict
  {-# INLINE toBEncode #-}

  fromBEncode = fromDict $ do
    FileInfo <$>! "length"
             <*>? "md5sum"
             <*>! "path"
  {-# INLINE fromBEncode #-}

type Put a = a -> BDict -> BDict

putFileInfoSingle :: Put (FileInfo ByteString)
putFileInfoSingle FileInfo {..} cont =
       "length" .=! fiLength
    .: "md5sum" .=? fiMD5Sum
    .: "name"   .=! fiName
    .: cont

getFileInfoSingle :: Get (FileInfo ByteString)
getFileInfoSingle = do
    FileInfo <$>! "length"
             <*>? "md5sum"
             <*>! "name"

instance BEncode (FileInfo ByteString) where
  toBEncode = toDict . (`putFileInfoSingle` endDict)
  {-# INLINE toBEncode #-}

  fromBEncode = fromDict getFileInfoSingle
  {-# INLINE fromBEncode #-}

{-----------------------------------------------------------------------
--  Original torrent file layout info
-----------------------------------------------------------------------}

-- | Original (found in torrent file) layout info is either:
--
--     * Single file with its /name/.
--
--     * Multiple files with its relative file /paths/.
--
data LayoutInfo
  = SingleFile
    { -- | Single file info.
      liFile     :: !(FileInfo ByteString)
    }
  | MultiFile
    { -- | List of the all files that torrent contains.
      liFiles    :: ![FileInfo [ByteString]]

      -- | The /suggested/ name of the root directory in which to
      -- store all the files.
    , liDirName  :: !ByteString
    } deriving (Show, Read, Eq, Typeable)

$(deriveJSON (L.map toLower . L.dropWhile isLower) ''LayoutInfo)

makeLensesFor
  [ ("liFile"   , "singleFile" )
  , ("liFiles"  , "multiFile"  )
  , ("liDirName", "rootDirName")
  ]
  ''LayoutInfo

instance NFData LayoutInfo where
  rnf SingleFile {..} = ()
  rnf MultiFile  {..} = rnf liFiles

getLayoutInfo :: Get LayoutInfo
getLayoutInfo = single <|> multi
  where
    single = SingleFile <$>  getFileInfoSingle
    multi  = MultiFile  <$>! "files" <*>! "name"

putLayoutInfo :: Put LayoutInfo
putLayoutInfo SingleFile {..} = putFileInfoSingle liFile
putLayoutInfo MultiFile  {..} = \ cont ->
     "files" .=! liFiles
  .: "name"  .=! liDirName
  .: cont

instance BEncode LayoutInfo where
  toBEncode   = toDict . (`putLayoutInfo` endDict)
  fromBEncode = fromDict getLayoutInfo

-- | Test if this is single file torrent.
isSingleFile :: LayoutInfo -> Bool
isSingleFile SingleFile {} = True
isSingleFile _             = False
{-# INLINE isSingleFile #-}

-- | Test if this is multifile torrent.
isMultiFile :: LayoutInfo -> Bool
isMultiFile MultiFile {} = True
isMultiFile _            = False
{-# INLINE isMultiFile #-}

-- | Find sum of sizes of the all torrent files.
contentLength :: LayoutInfo -> FileSize
contentLength SingleFile { liFile  = FileInfo {..} } = fiLength
contentLength MultiFile  { liFiles = tfs           } = sum (L.map fiLength tfs)

-- | Get number of all files in torrent.
fileCount :: LayoutInfo -> Int
fileCount SingleFile {..} = 1
fileCount MultiFile  {..} = L.length liFiles

-- | Find number of blocks of the specified size. If torrent size is
-- not a multiple of block size then the count is rounded up.
blockCount :: BlockSize -> LayoutInfo -> Int
blockCount blkSize ci = contentLength ci `sizeInBase` blkSize

{-----------------------------------------------------------------------
--  Flat layout
-----------------------------------------------------------------------}

-- | File layout specifies the order and the size of each file in the
--   storage. Note that order of files is highly important since we
--   coalesce all the files in the given order to get the linear block
--   address space.
--
type Layout a = [(FilePath, a)]

-- | Extract files layout from torrent info with the given root path.
flatLayout
  :: FilePath        -- ^ Root path for the all torrent files.
  -> LayoutInfo      -- ^ Torrent content information.
  -> Layout FileSize -- ^ The all file paths prefixed with the given root.
flatLayout prefixPath SingleFile { liFile = FileInfo {..} }
    = [(prefixPath </> BC.unpack fiName, fiLength)]
flatLayout prefixPath MultiFile  {..}     = L.map mkPath liFiles
  where   -- TODO use utf8 encoding in name
    mkPath FileInfo {..} = (path, fiLength)
      where
        path = prefixPath </> BC.unpack liDirName
           </> joinPath (L.map BC.unpack fiName)

-- | Calculate offset of each file based on its length, incrementally.
accumOffsets :: Layout FileSize -> Layout FileOffset
accumOffsets = go 0
  where
    go !_ [] = []
    go !offset ((n, s) : xs) = (n, offset) : go (offset + s) xs

-- | Gives global offset of a content file for a given full path.
fileOffset :: FilePath -> Layout FileOffset -> Maybe FileOffset
fileOffset = lookup
{-# INLINE fileOffset #-}

{-----------------------------------------------------------------------
-- Internal utilities
-----------------------------------------------------------------------}

-- | Divide and round up.
sizeInBase :: Integral a => a -> Int -> Int
sizeInBase n b = fromIntegral (n `div` fromIntegral b) + align
  where
    align = if n `mod` fromIntegral b == 0 then 0 else 1
{-# SPECIALIZE sizeInBase :: Int -> Int -> Int #-}
{-# SPECIALIZE sizeInBase :: Integer -> Int -> Int #-}
