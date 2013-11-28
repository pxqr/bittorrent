-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Infohash is a unique identifier of torrent.
--
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
module Data.Torrent.InfoHash
       ( InfoHash

         -- * Parsing
       , byteStringToInfoHash
       , textToInfoHash

         -- * Rendering
       , longHex
       , shortHex

       , addHashToURI
       ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.BEncode
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Base16 as Base16
import Data.ByteString.Base32 as Base32
import Data.ByteString.Base64 as Base64
import Data.Char
import Data.Convertible.Base
import Data.Default
import Data.List       as L
import Data.Maybe
import Data.Hashable   as Hashable
import Data.Serialize
import Data.String
import Data.Text as T
import Data.Text.Encoding as T
import Data.Typeable
import Network.HTTP.Types.QueryLike
import Network.URI
import Numeric
import Text.ParserCombinators.ReadP as P
import Text.PrettyPrint
import Text.PrettyPrint.Class


-- TODO
--
-- data Word160 = Word160 {-# UNPACK #-} !Word64
--                        {-# UNPACK #-} !Word64
--                        {-# UNPACK #-} !Word32
--
-- newtype InfoHash = InfoHash Word160
--
-- reason: bytestring have overhead = 8 words, while infohash have length 20 bytes

-- | Exactly 20 bytes long SHA1 hash of the info part of torrent file.
newtype InfoHash = InfoHash { getInfoHash :: BS.ByteString }
                   deriving (Eq, Ord, Typeable)

infoHashLen :: Int
infoHashLen = 20

-- | Meaningless placeholder value.
instance Default InfoHash where
  def = "0123456789012345678901234567890123456789"

-- | Hash raw bytes. (no encoding)
instance Hashable InfoHash where
  hash (InfoHash ih) = Hashable.hash ih
  {-# INLINE hash #-}

-- | Convert to\/from raw bencoded string. (no encoding)
instance BEncode InfoHash where
  toBEncode = toBEncode . getInfoHash
  fromBEncode be = InfoHash <$> fromBEncode be

-- | Convert to\/from raw bytestring. (no encoding)
instance Serialize InfoHash where
  put (InfoHash ih) = putByteString ih
  {-# INLINE put #-}

  get = InfoHash <$> getBytes 20
  {-# INLINE get #-}

-- | Convert to raw query value. (no encoding)
instance QueryValueLike InfoHash where
  toQueryValue (InfoHash ih) = Just ih
  {-# INLINE toQueryValue #-}

-- | Convert to base16 encoded string.
instance Show InfoHash where
  show (InfoHash ih) = BC.unpack (Base16.encode ih)

-- | Convert to base16 encoded Doc string.
instance Pretty InfoHash where
  pretty = text . show

-- | Read base16 encoded string.
instance Read InfoHash where
  readsPrec _ = readP_to_S $ do
      str <- replicateM 40 (satisfy isHexDigit)
      return $ InfoHash $ decodeIH str
    where
      decodeIH       = BS.pack . L.map fromHex . pair
      fromHex (a, b) = read $ '0' : 'x' : a : b : []

      pair (a : b : xs) = (a, b) : pair xs
      pair _            = []

-- | Convert raw bytes to info hash.
instance Convertible BS.ByteString InfoHash where
  safeConvert bs
    | BS.length bs == infoHashLen = pure (InfoHash bs)
    |          otherwise          = convError "invalid length" bs

-- | Parse infohash from base16\/base32\/base64 encoded string.
instance Convertible Text InfoHash where
  safeConvert t
      |      hashLen <= 28   =
        case Base64.decode hashStr of
          Left  msg   -> convError ("invalid base64 encoding" ++ msg) t
          Right ihStr -> pure $ InfoHash ihStr

      |      hashLen == 32   = pure $ InfoHash $ Base32.decode hashStr
-- TODO FIX Base32.decode can return 'undefined'

      |      hashLen == 40   =
        let (ihStr, inv) = Base16.decode hashStr
        in if BS.length inv == 0
           then pure $ InfoHash ihStr
           else convError "invalid base16 encoding" t

      |        otherwise     = convError "invalid length" t
    where
      hashLen = BS.length hashStr
      hashStr = T.encodeUtf8 t

-- | Decode from base16\/base32\/base64 encoded string.
instance IsString InfoHash where
  fromString str = fromMaybe err $ textToInfoHash $ T.pack str
    where
      err = error $ "fromString: invalid infohash string" ++ str

-- | Convert to base16 encoded JSON string.
instance ToJSON InfoHash where
  toJSON (InfoHash ih) = String $ T.decodeUtf8 $ Base16.encode ih

-- | Convert from base16\/base32\/base64 encoded JSON string.
instance FromJSON InfoHash where
  parseJSON = withText "JSON" $ -- TODO
    maybe (fail "could not parse InfoHash") pure . textToInfoHash

ignoreErrorMsg :: Either a b -> Maybe b
ignoreErrorMsg = either (const Nothing) Just

-- | TODO remove from API
byteStringToInfoHash :: BS.ByteString -> Maybe InfoHash
byteStringToInfoHash = ignoreErrorMsg . safeConvert

-- | Tries both base16 and base32 while decoding info hash. To Use
-- 'safeConvert' to find out
--
textToInfoHash :: Text -> Maybe InfoHash
textToInfoHash = ignoreErrorMsg . safeConvert

-- | Hex encode infohash to text, full length.
longHex :: InfoHash -> Text
longHex = T.decodeUtf8 . Base16.encode . getInfoHash

-- | The same as 'longHex', but 7 character long.
shortHex :: InfoHash -> Text
shortHex = T.take 7 . longHex

-- | TODO remove from API
--
--   Add query info hash parameter to uri.
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
