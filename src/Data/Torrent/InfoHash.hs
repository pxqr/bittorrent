module Data.Torrent.InfoHash
       ( InfoHash (getInfoHash)

         -- ^ Construction
       , hash, hashlazy

         -- ^ Extra
       , ppHex
       ) where

import Control.Applicative
import Data.Foldable
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as B
import qualified Data.ByteString.Lazy as Lazy
import Data.Serialize
import qualified Crypto.Hash.SHA1 as C

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
