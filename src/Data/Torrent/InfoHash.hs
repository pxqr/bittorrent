{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Torrent.InfoHash
       ( -- * Info hash
         InfoHash(..)
       , textToInfoHash
       , addHashToURI
       , ppInfoHash

       , Data.Torrent.InfoHash.hash
       , Data.Torrent.InfoHash.hashlazy
       ) where

import Control.Applicative
import Control.Monad
import qualified Crypto.Hash.SHA1 as C
import Data.Aeson
import Data.BEncode
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy  as BL
import Data.ByteString.Base16 as Base16
import Data.ByteString.Base32 as Base32
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy.Builder.ASCII as B
import Data.Char
import Data.List       as L
import Data.Hashable   as Hashable
import Data.URLEncoded as URL
import Data.Serialize
import Data.String
import Data.Text
import Data.Text.Encoding as T
import Network.URI
import Numeric
import Text.ParserCombinators.ReadP as P
import Text.PrettyPrint


-- | Exactly 20 bytes long SHA1 hash of the info part of torrent file.
newtype InfoHash = InfoHash { getInfoHash :: BS.ByteString }
                   deriving (Eq, Ord)

-- | for hex encoded strings
instance Show InfoHash where
  show = render . ppInfoHash

-- | for hex encoded strings
instance Read InfoHash where
  readsPrec _ = readP_to_S $ do
      str <- replicateM 40 (satisfy isHexDigit)
      return $ InfoHash $ decodeIH str
    where
      decodeIH       = BS.pack . L.map fromHex . pair
      fromHex (a, b) = read $ '0' : 'x' : a : b : []

      pair (a : b : xs) = (a, b) : pair xs
      pair _            = []

-- | for base16 (hex) encoded strings
instance IsString InfoHash where
  fromString str
    | L.length str == 40
    , (ihStr, inv) <- Base16.decode $ BC.pack str
    = if BS.length inv == 0 then InfoHash ihStr
                      else error "fromString: invalid infohash string"
    |       otherwise    = error "fromString: invalid infohash string length"

instance Hashable InfoHash where
  hash = Hashable.hash . getInfoHash

instance BEncode InfoHash where
  toBEncode = toBEncode . getInfoHash
  fromBEncode be = InfoHash <$> fromBEncode be

instance Serialize InfoHash where
  put = putByteString . getInfoHash
  get = InfoHash <$> getBytes 20

-- | Represented as base16 encoded string.
instance ToJSON InfoHash where
  toJSON (InfoHash ih) = String $ T.decodeUtf8 $ Base16.encode ih

-- | Can be base16 or base32 encoded string.
instance FromJSON InfoHash where
  parseJSON = withText "JSON" $
    maybe (fail "could not parse InfoHash") pure . textToInfoHash

instance URLShow InfoHash where
  urlShow = show

-- | Tries both base16 and base32 while decoding info hash.
textToInfoHash :: Text -> Maybe InfoHash
textToInfoHash text
    |      hashLen == 32   = Just $ InfoHash $ Base32.decode hashStr
    |      hashLen == 40   = let (ihStr, inv) = Base16.decode hashStr
                             in if BS.length inv == 0
                                then Just $ InfoHash ihStr
                                else Nothing
    |        otherwise     = Nothing
  where
    hashLen = BS.length hashStr
    hashStr = T.encodeUtf8 text

-- | Hash strict bytestring using SHA1 algorithm.
hash :: BS.ByteString -> InfoHash
hash = InfoHash . C.hash

-- | Hash lazy bytestring using SHA1 algorithm.
hashlazy :: BL.ByteString -> InfoHash
hashlazy = InfoHash . C.hashlazy

-- | Pretty print info hash in hexadecimal format.
ppInfoHash :: InfoHash -> Doc
ppInfoHash = text . BC.unpack . ppHex . getInfoHash

ppHex :: BS.ByteString -> BS.ByteString
ppHex = BL.toStrict . B.toLazyByteString . B.byteStringHexFixed

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
