module Data.Torrent.InfoHash
       ( InfoHash (getInfoHash)
       , addHashToURI

         -- ^ Construction
       , hash, hashlazy

         -- ^ Extra
       , ppHex
       ) where

import Control.Applicative
import Data.Foldable
import Data.List as L
import Data.Char
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as B
import qualified Data.ByteString.Lazy as Lazy
import Data.Serialize
import qualified Crypto.Hash.SHA1 as C
import Network.URI
import Numeric


-- | Exactly 20 bytes long SHA1 hash.
newtype InfoHash = InfoHash { getInfoHash :: ByteString }
                   deriving (Eq, Ord)

instance Show InfoHash where
  show = BC.unpack . ppHex

instance Serialize InfoHash where
  put = putByteString . getInfoHash
  get = InfoHash <$> getBytes 20

hash :: ByteString -> InfoHash
hash = InfoHash . C.hash

hashlazy :: Lazy.ByteString -> InfoHash
hashlazy = InfoHash . C.hashlazy

ppHex :: InfoHash -> ByteString
ppHex = Lazy.toStrict . B.toLazyByteString .
        foldMap (B.primFixed B.word8HexFixed) . B.unpack . getInfoHash

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
