-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Pieces are used to validate torrent content.
--
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Torrent.Piece
       ( -- * Piece attributes
         PieceIx
       , PieceCount
       , PieceSize
       , minPieceSize
       , maxPieceSize
       , defaultPieceSize

         -- * Piece data
       , Piece (..)
       , pieceSize

         -- * Piece control
       , HashArray (..)
       , PieceInfo (..)
       , pieceCount

         -- * Lens
       , pieceLength
       , pieceHashes

         -- * Validation
       , pieceHash
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
import Data.Int
import Data.Text.Encoding as T
import Data.Typeable
import Text.PrettyPrint
import Text.PrettyPrint.Class

import Data.Torrent.JSON


-- TODO add torrent file validation
class Lint a where
   lint :: a -> Either String a

--class Validation a where
--  validate :: PieceInfo -> Piece a -> Bool

{-----------------------------------------------------------------------
-- Piece attributes
-----------------------------------------------------------------------}

-- | Zero-based index of piece in torrent content.
type PieceIx   = Int

-- | Size of piece in bytes. Should be a power of 2.
--
--   NOTE: Have max and min size constrained to wide used
--   semi-standard values. This bounds should be used to make decision
--   about piece size for new torrents.
--
type PieceSize = Int

-- | Number of pieces in torrent or a part of torrent.
type PieceCount = Int

defaultBlockSize :: Int
defaultBlockSize = 16 * 1024

-- | Optimal number of pieces in torrent.
optimalPieceCount :: PieceCount
optimalPieceCount = 1000
{-# INLINE optimalPieceCount #-}

-- | Piece size should not be less than this value.
minPieceSize :: Int
minPieceSize = defaultBlockSize * 4
{-# INLINE minPieceSize #-}

-- | To prevent transfer degradation piece size should not exceed this
-- value.
maxPieceSize :: Int
maxPieceSize = 4 * 1024 * 1024
{-# INLINE maxPieceSize #-}

toPow2 :: Int -> Int
toPow2 x = bit $ fromIntegral (leadingZeros (0 :: Int) - leadingZeros x)

-- | Find the optimal piece size for a given torrent size.
defaultPieceSize :: Int64 -> Int
defaultPieceSize x = max minPieceSize $ min maxPieceSize $ toPow2 pc
  where
    pc = fromIntegral (x `div` fromIntegral optimalPieceCount)

{-----------------------------------------------------------------------
-- Piece data
-----------------------------------------------------------------------}

-- TODO check if pieceLength is power of 2
-- | Piece payload should be strict or lazy bytestring.
data Piece a = Piece
  { -- | Zero-based piece index in torrent.
    pieceIndex :: {-# UNPACK #-} !PieceIx

    -- | Payload.
  , pieceData  :: !a
  } deriving (Show, Read, Eq, Functor, Typeable)

$(deriveJSON omitRecordPrefix ''Piece)

instance NFData (Piece a)

-- | Payload bytes are omitted.
instance Pretty (Piece a) where
  pretty Piece {..} = "Piece" <+> braces ("index" <+> "=" <+> int pieceIndex)

-- | Get size of piece in bytes.
pieceSize :: Piece BL.ByteString -> PieceSize
pieceSize Piece {..} = fromIntegral (BL.length pieceData)

{-----------------------------------------------------------------------
-- Piece control
-----------------------------------------------------------------------}

-- | A flat array of SHA1 sums of each piece.
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

$(deriveJSON omitRecordPrefix ''PieceInfo)

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

-- | Hashes are omitted.
instance Pretty PieceInfo where
  pretty  PieceInfo {..} = "Piece size: " <> int piPieceLength

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

-- | Test if this is last piece in torrent content.
isLastPiece :: PieceInfo -> PieceIx -> Bool
isLastPiece ci i = pieceCount ci == succ i

-- | Validate piece with metainfo hash.
checkPieceLazy :: PieceInfo -> Piece BL.ByteString -> Bool
checkPieceLazy pinfo @ PieceInfo {..} Piece {..}
  = (fromIntegral (BL.length pieceData) == piPieceLength
     || isLastPiece pinfo pieceIndex)
  && SHA1.hashlazy pieceData == pieceHash pinfo pieceIndex
