-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Torrent.Piece
       ( -- * Piece attributes
         -- ** Piece size
         PieceSize (..)
       , defaultBlockSize -- TODO use data-default
       , optimalPieceCount
       , defaultPieceSize -- TODO use data-default

         -- ** Piece index
       , PieceIx

         -- * Piece data
       , Piece (..)
       , ppPiece

         -- * Piece control
       , PieceInfo (..)
       , ppPieceInfo
       , pieceLength
       , pieceHashes
       , pieceHash
       , pieceCount
       , checkPieceLazy


         -- * Internal
       , getPieceInfo
       , putPieceInfo
       ) where

import Control.DeepSeq
import Control.Lens
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), withText)
import Data.Aeson.TH
import Data.BEncode
import Data.BEncode.Types
import Data.Bits
import Data.Bits.Extras
import           Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import Data.Char
import Data.Int
import Data.List as L
import Data.Text.Encoding as T
import Data.Typeable
import Text.PrettyPrint


class Lint a where
  lint :: a -> Either String a

type PieceCount = Int -- TODO newtype
type PieceIx = Int    -- TODO remove

newtype PieceIndex = PieceIndex Int

-- | An int used to denote piece size.
newtype PieceSize = PieceSize Int
  deriving (Show, Read, Typeable
           , Eq, Ord, Enum
           , Num, Real, Integral
           , BEncode, ToJSON, FromJSON
           )

-- | Widely used semi-official block size.
defaultBlockSize :: Int
defaultBlockSize = 16 * 1024

maxPieceSize :: Int
maxPieceSize = 4 * 1024 * 1024
{-# INLINE maxPieceSize #-}

minPieceSize :: Int
minPieceSize = defaultBlockSize * 4
{-# INLINE minPieceSize #-}

-- | NOTE: Have max and min size constrained to wide used
-- semi-standard values. This bounds should be used to make decision
-- about piece size for new torrents.
--
instance Bounded PieceSize where
  maxBound = PieceSize maxPieceSize
  {-# INLINE maxBound #-}

  minBound = PieceSize minPieceSize
  {-# INLINE minBound #-}

-- | TODO
optimalPieceCount :: Int
optimalPieceCount = 1000
{-# INLINE optimalPieceCount #-}

toPow2 :: Int -> Int
toPow2 x = bit $ fromIntegral (leadingZeros (0 :: Int) - leadingZeros x)

-- | Find the optimal piece size for a given torrent size.
defaultPieceSize :: Int64 -> Int
defaultPieceSize x = max minPieceSize $ min maxPieceSize $ toPow2 pc
  where
    pc = fromIntegral (x `div` fromIntegral optimalPieceCount)

-- TODO check if pieceLength is power of 2
-- | Piece payload should be strict or lazy bytestring.
data Piece a = Piece
  { -- | Zero-based piece index in torrent. TODO how pieces are indexed?
    pieceIndex :: {-# UNPACK #-} !PieceIx
    -- | Payload.
  , pieceData  :: !a
  } deriving (Show, Read, Eq, Typeable)

$(deriveJSON (L.map toLower . L.dropWhile isLower) ''Piece)

instance NFData (Piece a)

-- | Format piece in human readable form. Payload bytes are omitted.
ppPiece :: Piece a -> Doc
ppPiece Piece {..}
  = "Piece" <+> braces ("index" <+> "=" <+> int pieceIndex)

newtype HashArray = HashArray { unHashArray :: ByteString }
                    deriving (Show, Read, Eq, BEncode)

-- | Represented as base64 encoded JSON string.
instance ToJSON HashArray where
  toJSON (HashArray bs) = String $ T.decodeUtf8 $ Base64.encode bs

instance FromJSON HashArray where
  parseJSON = withText "HashArray" $
    either fail (return . HashArray) . Base64.decode . T.encodeUtf8

-- | Part of torrent file used for torrent content validation.
data PieceInfo = PieceInfo
  { piPieceLength  :: {-# UNPACK #-} !PieceSize
    -- ^ Number of bytes in each piece.

  , piPieceHashes  :: !HashArray
    -- ^ Concatenation of all 20-byte SHA1 hash values.
  } deriving (Show, Read, Eq, Typeable)

$(deriveJSON (L.map toLower . L.dropWhile isLower) ''PieceInfo)

-- | Number of bytes in each piece.
makeLensesFor [("piPieceLength", "pieceLength")] ''PieceInfo

-- | Concatenation of all 20-byte SHA1 hash values.
makeLensesFor [("piPieceHashes", "pieceHashes")] ''PieceInfo

instance NFData PieceInfo

instance Lint PieceInfo where
  lint pinfo @ PieceInfo {..}
    | BS.length (unHashArray piPieceHashes) `rem` hashsize == 0
    , piPieceLength >= 0 = return pinfo
    |       otherwise    = Left undefined


putPieceInfo :: PieceInfo -> BDict -> BDict
putPieceInfo PieceInfo {..} cont =
       "piece length" .=! piPieceLength
    .: "pieces"       .=! piPieceHashes
    .: cont

getPieceInfo :: Get PieceInfo
getPieceInfo = do
    PieceInfo <$>! "piece length"
              <*>! "pieces"

instance BEncode PieceInfo where
  toBEncode   = toDict . (`putPieceInfo` endDict)
  fromBEncode = fromDict getPieceInfo

-- | Format piece info in human readable form. Hashes are omitted.
ppPieceInfo :: PieceInfo -> Doc
ppPieceInfo PieceInfo { piPieceLength = PieceSize len } =
  "PieceInfo" <+> braces ("length" <+> "=" <+> int len)

hashsize :: Int
hashsize = 20
{-# INLINE hashsize #-}

slice :: Int -> Int -> ByteString -> ByteString
slice start len = BS.take len . BS.drop start
{-# INLINE slice #-}

-- | Extract validation hash by specified piece index.
pieceHash :: PieceInfo -> PieceIx -> ByteString
pieceHash PieceInfo {..} i = slice (hashsize * i) hashsize (unHashArray piPieceHashes)

-- | Find count of pieces in the torrent. If torrent size is not a
-- multiple of piece size then the count is rounded up.
pieceCount :: PieceInfo -> PieceCount
pieceCount PieceInfo {..} = BS.length (unHashArray piPieceHashes) `quot` hashsize

isLastPiece :: PieceInfo -> PieceIx -> Bool
isLastPiece ci i = pieceCount ci == succ i

class Validation a where
  validate :: PieceInfo -> Piece a -> Bool

-- | Validate piece with metainfo hash.
checkPieceLazy :: PieceInfo -> Piece BL.ByteString -> Bool
checkPieceLazy pinfo @ PieceInfo {..} Piece {..}
  = (fromIntegral (BL.length pieceData) == piPieceLength
     || isLastPiece pinfo pieceIndex)
  && SHA1.hashlazy pieceData == pieceHash pinfo pieceIndex
