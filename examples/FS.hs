{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Arrow
import Data.ByteString.Char8 as BC
import Data.List as L
import Data.Map as M
import Data.Torrent as T
import Data.Torrent.Tree as T
import System.Environment
import System.Fuse
import System.FilePath
import System.Posix.Files


defStat :: FileStat
defStat = FileStat
  { statEntryType        = Unknown
  , statFileMode         = ownerReadMode
  , statLinkCount        = 2

  , statFileOwner        = 0
  , statFileGroup        = 0

  , statSpecialDeviceID  = 0

  , statFileSize         = 0
  , statBlocks           = 0

  , statAccessTime       = 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }

dirStat :: FileStat
dirStat = defStat {
    statEntryType = Directory
  }

type Result a = IO (Either Errno a)
type Result'  = IO Errno

fsGetFileStat :: Torrent -> FilePath -> Result FileStat
fsGetFileStat _ path = return $ Right dirStat

fsOpenDirectory :: Torrent -> FilePath -> Result'
fsOpenDirectory _ _ = return eOK

fsReadDirectory :: Torrent -> FilePath -> Result [(FilePath, FileStat)]
fsReadDirectory Torrent {tInfoDict = InfoDict {..}} path
    | Just cs <- T.lookupDir (L.tail (splitDirectories path)) tree =
      return $ Right $ L.map (BC.unpack *** const defStat) cs
    | otherwise = return $ Left eNOENT
  where
    tree = build $ idLayoutInfo

fsReleaseDirectory :: Torrent -> FilePath -> Result'
fsReleaseDirectory _ _ = return eOK

exfsOps :: Torrent -> FuseOperations ()
exfsOps t = defaultFuseOps
  { fuseGetFileStat        = fsGetFileStat t

  , fuseOpenDirectory      = fsOpenDirectory t
  , fuseReadDirectory      = fsReadDirectory t
  , fuseReleaseDirectory   = fsReleaseDirectory t
  }

main :: IO ()
main = do
  x : xs <- getArgs
  t <- fromFile x
  withArgs xs $ do
    fuseMain (exfsOps t) defaultExceptionHandler