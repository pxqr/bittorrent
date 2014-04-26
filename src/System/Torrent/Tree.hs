-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Directory tree can be used to easily manipulate file layout info.
--
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module System.Torrent.Tree
       ( -- * Directory tree
         DirTree (..)

         -- * Construction
       , build

         -- * Query
       , System.Torrent.Tree.lookup
       , lookupDir
       , fileNumber
       , dirNumber
       ) where

import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.Foldable
import Data.List as L
import Data.Map  as M
import Data.Monoid

import Data.Torrent


-- | 'DirTree' is more convenient form of 'LayoutInfo'.
data DirTree a = Dir  { children :: Map ByteString (DirTree a) }
               | File { node     :: FileInfo a                 }
                 deriving Show

-- | Build directory tree from a list of files.
build :: LayoutInfo -> DirTree ()
build SingleFile {liFile = FileInfo {..}} = Dir
    { children = M.singleton fiName (File fi) }
  where
    fi = FileInfo fiLength fiMD5Sum ()
build MultiFile {..} = Dir $ M.singleton liDirName files
  where
    files = Dir $ M.fromList $ L.map mkFileEntry liFiles
    mkFileEntry FileInfo {..} = (L.head fiName, ent) -- TODO FIXME
      where
        ent = File $ FileInfo fiLength fiMD5Sum ()

--decompress :: DirTree () -> [FileInfo ()]
--decompress = undefined

-- TODO pretty print

-- | Lookup file by path.
lookup :: [FilePath] -> DirTree a -> Maybe (DirTree a)
lookup []        t      = Just t
lookup (p : ps) (Dir m) | Just subTree <- M.lookup (BC.pack p) m
                        = System.Torrent.Tree.lookup ps subTree
lookup _         _      = Nothing

-- | Lookup directory by path.
lookupDir :: [FilePath] -> DirTree a -> Maybe [(ByteString, DirTree a)]
lookupDir ps d = do
  subTree <- System.Torrent.Tree.lookup ps d
  case subTree of
    File _  -> Nothing
    Dir  es -> Just $ M.toList es

-- | Get total count of files in directory and subdirectories.
fileNumber :: DirTree a -> Sum Int
fileNumber File {..} = Sum 1
fileNumber Dir  {..} = foldMap fileNumber children

-- | Get total count of directories in the directory and subdirectories.
dirNumber :: DirTree a -> Sum Int
dirNumber File {..} = Sum 0
dirNumber Dir  {..} = Sum 1 <> foldMap dirNumber children
