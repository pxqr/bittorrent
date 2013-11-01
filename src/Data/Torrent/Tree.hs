-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Torrent.Tree
       ( DirTree (..)
       , build

       , Data.Torrent.Tree.lookup
       , lookupDir

       , fileNumber
       , dirNumber
       ) where

import Control.Arrow
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.Foldable
import Data.List as L
import Data.Map  as M
import Data.Monoid

import Data.Torrent.Layout


data DirTree a = Dir  { children :: Map ByteString (DirTree a) }
               | File { node     :: FileInfo a                 }
                 deriving Show

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

decompress :: DirTree () -> [FileInfo ()]
decompress = undefined

lookup :: [FilePath] -> DirTree a -> Maybe (DirTree a)
lookup []        t      = Just t
lookup (p : ps) (Dir m) | Just subTree <- M.lookup (BC.pack p) m
                        = Data.Torrent.Tree.lookup ps subTree
lookup _         _      = Nothing

lookupDir :: [FilePath] -> DirTree a -> Maybe [(ByteString, DirTree a)]
lookupDir ps d
  | Just subTree <- Data.Torrent.Tree.lookup ps d =
    case subTree of
      File _  -> Nothing
      Dir  es -> Just $ M.toList es

fileNumber :: DirTree a -> Sum Int
fileNumber File {..} = Sum 1
fileNumber Dir  {..} = foldMap fileNumber children

dirNumber :: DirTree a -> Sum Int
dirNumber File {..} = Sum 0
dirNumber Dir  {..} = Sum 1 <> foldMap dirNumber children
