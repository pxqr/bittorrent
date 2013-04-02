{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Data.Foldable
import Data.Maybe
import qualified Data.Map as M
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as B

import System.Environment

import Data.Torrent
import Data.BEncode


checkInfo :: B.ByteString
checkInfo = "0221caf96aa3cb94f0f58d458e78b0fc344ad8bf"

-- | _should_ be 'id' if content is correct
reencode :: B.ByteString -> Result L.ByteString
reencode content = (encode . toBEncode . (`asTypeOf` (undefined :: Torrent)))
            `fmap` (fromBEncode =<< decode content)

ppHex :: B.ByteString -> B.ByteString
ppHex = L.toStrict . B.toLazyByteString . foldMap (B.primFixed B.word8HexFixed) . B.unpack

chunk :: Int -> B.ByteString -> [B.ByteString]
chunk size b | B.length b == 0 = [b]
             | otherwise       =
               let (x, xs) = B.splitAt size b
               in x : chunk size xs

showInfos :: ByteString -> (TorrentInfo, BEncode)
showInfos bs =
  let Right (be@(BDict dict)) = decode bs
      Right t = tInfo <$> (fromBEncode be)
      orig = BDict $ let BDict info = fromJust (M.lookup "info" dict)
                     in M.insert "pieces" (BString "") info
  in (t { tPieces = "" }, orig)

main :: IO ()
main = do
  [path] <- getArgs
  Right t <- fromFile path
  B.putStrLn (ppHex (tInfoHash t))
