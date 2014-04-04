-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Torrent file contains metadata about files and folders but not
--   content itself. The files are bencoded dictionaries. There is
--   also other info which is used to help join the swarm.
--
--   This module provides torrent metainfo serialization and info hash
--   extraction.
--
--   For more info see:
--   <http://www.bittorrent.org/beps/bep_0003.html#metainfo-files>,
--   <https://wiki.theory.org/BitTorrentSpecification#Metainfo_File_Structure>
--
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -fno-warn-orphans           #-}
module Data.Torrent
       ( -- * InfoHash
         -- $infohash
         InfoHash
       , textToInfoHash
       , longHex
       , shortHex

         -- * File layout
         -- ** FileInfo
       , FileOffset
       , FileSize
       , FileInfo (..)
       , fileLength
       , filePath
       , fileMD5Sum

         -- ** Layout info
       , LayoutInfo (..)
       , singleFile
       , multiFile
       , rootDirName
       , joinFilePath
       , isSingleFile
       , isMultiFile
       , suggestedName
       , contentLength
       , fileCount
       , blockCount

         -- ** Flat layout info
       , FileLayout
       , flatLayout
       , accumPositions
       , fileOffset

         -- ** Internal
       , sizeInBase

         -- * Pieces
         -- ** Attributes
       , PieceIx
       , PieceCount
       , PieceSize
       , minPieceSize
       , maxPieceSize
       , defaultPieceSize
       , PieceHash

         -- ** Piece data
       , Piece (..)
       , pieceSize
       , hashPiece

         -- ** Piece control
       , HashList (..)
       , PieceInfo (..)
       , pieceLength
       , pieceHashes
       , pieceCount

         -- ** Validation
       , pieceHash
       , checkPieceLazy

         -- * Info dictionary
       , InfoDict (..)
       , infohash
       , layoutInfo
       , pieceInfo
       , isPrivate
       , infoDictionary

         -- * Torrent file
       , Torrent(..)

         -- ** Lenses
       , announce
       , announceList
       , comment
       , createdBy
       , creationDate
       , encoding
       , infoDict
       , publisher
       , publisherURL
       , signature

         -- ** Utils
       , nullTorrent
       , typeTorrent
       , torrentExt
       , isTorrentPath
       , fromFile
       , toFile

         -- * Magnet
         -- $magnet-link
       , Magnet(..)
       , nullMagnet
       , simpleMagnet
       , detailedMagnet
       , parseMagnet
       , renderMagnet

         -- ** URN
       , URN (..)
       , NamespaceId
       , btih
       , infohashURN
       , parseURN
       , renderURN
       ) where

import Prelude hiding (sum)
import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Lens hiding (unsnoc)
import Control.Monad
import qualified Crypto.Hash.SHA1 as C
import qualified Crypto.Hash.SHA1 as SHA1
import Data.BEncode as BE
import Data.BEncode.Types as BE
import Data.Bits
import Data.Bits.Extras
import           Data.ByteString as BS
import           Data.ByteString.Base16 as Base16
import           Data.ByteString.Base32 as Base32
import           Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.ByteString.Lazy  as BL
import           Data.Char
import           Data.Convertible
import           Data.Default
import           Data.Foldable as F
import           Data.Hashable   as Hashable
import           Data.Int
import qualified Data.List as L
import           Data.Map as M
import           Data.Maybe
import           Data.Serialize as S
import           Data.String
import           Data.Text as T
import           Data.Text.Encoding as T
import           Data.Text.Read
import Data.Time.Clock.POSIX
import Data.Typeable
import Network (HostName)
import Network.HTTP.Types.QueryLike
import Network.HTTP.Types.URI
import Network.URI
import Text.ParserCombinators.ReadP as P
import Text.PrettyPrint as PP
import Text.PrettyPrint.Class
import System.FilePath
import System.Posix.Types

import Network.BitTorrent.Core.NodeInfo


{-----------------------------------------------------------------------
--  Info hash
-----------------------------------------------------------------------}
-- TODO
--
-- data Word160 = Word160 {-# UNPACK #-} !Word64
--                        {-# UNPACK #-} !Word64
--                        {-# UNPACK #-} !Word32
--
-- newtype InfoHash = InfoHash Word160
--
-- reason: bytestring have overhead = 8 words, while infohash have length 20 bytes

-- $infohash
--
-- Infohash is a unique identifier of torrent.

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
  hashWithSalt s (InfoHash ih) = hashWithSalt s ih
  {-# INLINE hashWithSalt #-}

-- | Convert to\/from raw bencoded string. (no encoding)
instance BEncode InfoHash where
  toBEncode = toBEncode . getInfoHash
  fromBEncode be = InfoHash <$> fromBEncode be

-- | Convert to\/from raw bytestring. (no encoding)
instance Serialize InfoHash where
  put (InfoHash ih) = putByteString ih
  {-# INLINE put #-}

  get = InfoHash <$> getBytes infoHashLen
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
      str <- replicateM (infoHashLen * 2) (satisfy isHexDigit)
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
      | 20 == hashLen = pure (InfoHash hashStr)
      | 26 <= hashLen && hashLen <= 28 =
        case Base64.decode hashStr of
          Left  msg   -> convError ("invalid base64 encoding " ++ msg) t
          Right ihStr -> safeConvert ihStr

      |      hashLen == 32   =
        case Base32.decode hashStr of
          Left  msg   -> convError msg t
          Right ihStr -> safeConvert ihStr

      |      hashLen == 40   =
        let (ihStr, inv) = Base16.decode hashStr
        in if BS.length inv /= 0
           then convError "invalid base16 encoding" t
           else safeConvert ihStr

      |        otherwise     = convError "invalid length" t
    where
      hashLen = BS.length hashStr
      hashStr = T.encodeUtf8 t

-- | Decode from base16\/base32\/base64 encoded string.
instance IsString InfoHash where
  fromString = either (error . prettyConvertError) id . safeConvert . T.pack

ignoreErrorMsg :: Either a b -> Maybe b
ignoreErrorMsg = either (const Nothing) Just

-- | Tries both base16 and base32 while decoding info hash.
--
--  Use 'safeConvert' for detailed error messages.
--
textToInfoHash :: Text -> Maybe InfoHash
textToInfoHash = ignoreErrorMsg . safeConvert

-- | Hex encode infohash to text, full length.
longHex :: InfoHash -> Text
longHex = T.decodeUtf8 . Base16.encode . getInfoHash

-- | The same as 'longHex', but only first 7 characters.
shortHex :: InfoHash -> Text
shortHex = T.take 7 . longHex

{-----------------------------------------------------------------------
--  File info
-----------------------------------------------------------------------}

-- | Size of a file in bytes.
type FileSize = FileOffset

deriving instance BEncode  FileOffset

-- | Contain metainfo about one single file.
data FileInfo a = FileInfo {
      fiLength      :: {-# UNPACK #-} !FileSize
      -- ^ Length of the file in bytes.

      -- TODO unpacked MD5 sum
    , fiMD5Sum      :: !(Maybe ByteString)
      -- ^ 32 character long MD5 sum of the file.  Used by third-party
      -- tools, not by bittorrent protocol itself.

    , fiName        :: !a
      -- ^ One or more string elements that together represent the
      -- path and filename. Each element in the list corresponds to
      -- either a directory name or (in the case of the last element)
      -- the filename.  For example, the file:
      --
      --   > "dir1/dir2/file.ext"
      --
      --   would consist of three string elements:
      --
      --   > ["dir1", "dir2", "file.ext"]
      --
    } deriving (Show, Read, Eq, Typeable
               , Functor, Foldable
               )

makeLensesFor
  [ ("fiLength", "fileLength")
  , ("fiMD5Sum", "fileMD5Sum")
  , ("fiName"  , "filePath"  )
  ]
  ''FileInfo

instance NFData a => NFData (FileInfo a) where
  rnf FileInfo {..} = rnf fiName
  {-# INLINE rnf #-}

instance BEncode (FileInfo [ByteString]) where
  toBEncode FileInfo {..} = toDict $
       "length" .=! fiLength
    .: "md5sum" .=? fiMD5Sum
    .: "path"   .=! fiName
    .: endDict
  {-# INLINE toBEncode #-}

  fromBEncode = fromDict $ do
    FileInfo <$>! "length"
             <*>? "md5sum"
             <*>! "path"
  {-# INLINE fromBEncode #-}

type Put a = a -> BDict -> BDict

putFileInfoSingle :: Data.Torrent.Put (FileInfo ByteString)
putFileInfoSingle FileInfo {..} cont =
       "length" .=! fiLength
    .: "md5sum" .=? fiMD5Sum
    .: "name"   .=! fiName
    .: cont

getFileInfoSingle :: BE.Get (FileInfo ByteString)
getFileInfoSingle = do
    FileInfo <$>! "length"
             <*>? "md5sum"
             <*>! "name"

instance BEncode (FileInfo ByteString) where
  toBEncode = toDict . (`putFileInfoSingle` endDict)
  {-# INLINE toBEncode #-}

  fromBEncode = fromDict getFileInfoSingle
  {-# INLINE fromBEncode #-}

instance Pretty (FileInfo BS.ByteString) where
  pretty FileInfo {..} =
       "Path: " <> text (T.unpack (T.decodeUtf8 fiName))
    $$ "Size: " <> text (show fiLength)
    $$ maybe PP.empty ppMD5 fiMD5Sum
   where
    ppMD5 md5 = "MD5 : " <> text (show (Base16.encode md5))

-- | Join file path.
joinFilePath :: FileInfo [ByteString] -> FileInfo ByteString
joinFilePath = fmap (BS.intercalate "/")

{-----------------------------------------------------------------------
--  Layout info
-----------------------------------------------------------------------}

-- | Original (found in torrent file) layout info is either:
--
--     * Single file with its /name/.
--
--     * Multiple files with its relative file /paths/.
--
data LayoutInfo
  = SingleFile
    { -- | Single file info.
      liFile     :: !(FileInfo ByteString)
    }
  | MultiFile
    { -- | List of the all files that torrent contains.
      liFiles    :: ![FileInfo [ByteString]]

      -- | The /suggested/ name of the root directory in which to
      -- store all the files.
    , liDirName  :: !ByteString
    } deriving (Show, Read, Eq, Typeable)

makeLensesFor
  [ ("liFile"   , "singleFile" )
  , ("liFiles"  , "multiFile"  )
  , ("liDirName", "rootDirName")
  ]
  ''LayoutInfo

instance NFData LayoutInfo where
  rnf SingleFile {..} = ()
  rnf MultiFile  {..} = rnf liFiles

-- | Empty multifile layout.
instance Default LayoutInfo where
  def = MultiFile [] ""

getLayoutInfo :: BE.Get LayoutInfo
getLayoutInfo = single <|> multi
  where
    single = SingleFile <$>  getFileInfoSingle
    multi  = MultiFile  <$>! "files" <*>! "name"

putLayoutInfo :: Data.Torrent.Put LayoutInfo
putLayoutInfo SingleFile {..} = putFileInfoSingle liFile
putLayoutInfo MultiFile  {..} = \ cont ->
     "files" .=! liFiles
  .: "name"  .=! liDirName
  .: cont

instance BEncode LayoutInfo where
  toBEncode   = toDict . (`putLayoutInfo` endDict)
  fromBEncode = fromDict getLayoutInfo

instance Pretty LayoutInfo where
  pretty SingleFile {..} = pretty liFile
  pretty MultiFile  {..} = vcat $ L.map (pretty . joinFilePath) liFiles

-- | Test if this is single file torrent.
isSingleFile :: LayoutInfo -> Bool
isSingleFile SingleFile {} = True
isSingleFile _             = False
{-# INLINE isSingleFile #-}

-- | Test if this is multifile torrent.
isMultiFile :: LayoutInfo -> Bool
isMultiFile MultiFile {} = True
isMultiFile _            = False
{-# INLINE isMultiFile #-}

-- | Get name of the torrent based on the root path piece.
suggestedName :: LayoutInfo -> ByteString
suggestedName (SingleFile FileInfo {..}) = fiName
suggestedName  MultiFile           {..}  = liDirName
{-# INLINE suggestedName #-}

-- | Find sum of sizes of the all torrent files.
contentLength :: LayoutInfo -> FileSize
contentLength SingleFile { liFile  = FileInfo {..} } = fiLength
contentLength MultiFile  { liFiles = tfs           } = L.sum (L.map fiLength tfs)

-- | Get number of all files in torrent.
fileCount :: LayoutInfo -> Int
fileCount SingleFile {..} = 1
fileCount MultiFile  {..} = L.length liFiles

-- | Find number of blocks of the specified size. If torrent size is
-- not a multiple of block size then the count is rounded up.
blockCount :: Int -> LayoutInfo -> Int
blockCount blkSize ci = contentLength ci `sizeInBase` blkSize

------------------------------------------------------------------------

-- | File layout specifies the order and the size of each file in the
--   storage. Note that order of files is highly important since we
--   coalesce all the files in the given order to get the linear block
--   address space.
--
type FileLayout a = [(FilePath, a)]

-- | Extract files layout from torrent info with the given root path.
flatLayout
  :: FilePath            -- ^ Root path for the all torrent files.
  -> LayoutInfo          -- ^ Torrent content information.
  -> FileLayout FileSize -- ^ The all file paths prefixed with the given root.
flatLayout prefixPath SingleFile { liFile = FileInfo {..} }
    = [(prefixPath </> BC.unpack fiName, fiLength)]
flatLayout prefixPath MultiFile  {..}     = L.map mkPath liFiles
  where   -- TODO use utf8 encoding in name
    mkPath FileInfo {..} = (path, fiLength)
      where
        path = prefixPath </> BC.unpack liDirName
           </> joinPath (L.map BC.unpack fiName)

-- | Calculate offset of each file based on its length, incrementally.
accumPositions :: FileLayout FileSize -> FileLayout (FileOffset, FileSize)
accumPositions = go 0
  where
    go !_ [] = []
    go !offset ((n, s) : xs) = (n, (offset, s)) : go (offset + s) xs

-- | Gives global offset of a content file for a given full path.
fileOffset :: FilePath -> FileLayout FileOffset -> Maybe FileOffset
fileOffset = L.lookup
{-# INLINE fileOffset #-}

------------------------------------------------------------------------

-- | Divide and round up.
sizeInBase :: Integral a => a -> Int -> Int
sizeInBase n b = fromIntegral (n `div` fromIntegral b) + align
  where
    align = if n `mod` fromIntegral b == 0 then 0 else 1
{-# SPECIALIZE sizeInBase :: Int -> Int -> Int #-}
{-# SPECIALIZE sizeInBase :: Integer -> Int -> Int #-}

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

type PieceHash = ByteString

hashsize :: Int
hashsize = 20
{-# INLINE hashsize #-}

-- TODO check if pieceLength is power of 2
-- | Piece payload should be strict or lazy bytestring.
data Piece a = Piece
  { -- | Zero-based piece index in torrent.
    pieceIndex :: {-# UNPACK #-} !PieceIx

    -- | Payload.
  , pieceData  :: !a
  } deriving (Show, Read, Eq, Functor, Typeable)

instance NFData (Piece a)

-- | Payload bytes are omitted.
instance Pretty (Piece a) where
  pretty Piece {..} = "Piece" <+> braces ("index" <+> "=" <+> int pieceIndex)

-- | Get size of piece in bytes.
pieceSize :: Piece BL.ByteString -> PieceSize
pieceSize Piece {..} = fromIntegral (BL.length pieceData)

-- | Get piece hash.
hashPiece :: Piece BL.ByteString -> PieceHash
hashPiece Piece {..} = SHA1.hashlazy pieceData

{-----------------------------------------------------------------------
-- Piece control
-----------------------------------------------------------------------}

-- | A flat array of SHA1 hash for each piece.
newtype HashList = HashList { unHashList :: ByteString }
  deriving (Show, Read, Eq, BEncode, Typeable)

-- | Empty hash list.
instance Default HashList where
  def = HashList ""

-- | Part of torrent file used for torrent content validation.
data PieceInfo = PieceInfo
  { piPieceLength  :: {-# UNPACK #-} !PieceSize
    -- ^ Number of bytes in each piece.

  , piPieceHashes  :: !HashList
    -- ^ Concatenation of all 20-byte SHA1 hash values.
  } deriving (Show, Read, Eq, Typeable)

-- | Number of bytes in each piece.
makeLensesFor [("piPieceLength", "pieceLength")] ''PieceInfo

-- | Concatenation of all 20-byte SHA1 hash values.
makeLensesFor [("piPieceHashes", "pieceHashes")] ''PieceInfo

instance NFData PieceInfo

instance Default PieceInfo where
  def = PieceInfo 1 def

class Lint a where
   lint :: a -> Either String a

instance Lint PieceInfo where
  lint pinfo @ PieceInfo {..}
    | BS.length (unHashList piPieceHashes) `rem` hashsize == 0
    , piPieceLength >= 0 = return pinfo
    |       otherwise    = Left undefined


putPieceInfo :: Data.Torrent.Put PieceInfo
putPieceInfo PieceInfo {..} cont =
       "piece length" .=! piPieceLength
    .: "pieces"       .=! piPieceHashes
    .: cont

getPieceInfo :: BE.Get PieceInfo
getPieceInfo = do
    PieceInfo <$>! "piece length"
              <*>! "pieces"

instance BEncode PieceInfo where
  toBEncode   = toDict . (`putPieceInfo` endDict)
  fromBEncode = fromDict getPieceInfo

-- | Hashes are omitted.
instance Pretty PieceInfo where
  pretty  PieceInfo {..} = "Piece size: " <> int piPieceLength

slice :: Int -> Int -> ByteString -> ByteString
slice start len = BS.take len . BS.drop start
{-# INLINE slice #-}

-- | Extract validation hash by specified piece index.
pieceHash :: PieceInfo -> PieceIx -> PieceHash
pieceHash PieceInfo {..} i = slice (hashsize * i) hashsize (unHashList piPieceHashes)

-- | Find count of pieces in the torrent. If torrent size is not a
-- multiple of piece size then the count is rounded up.
pieceCount :: PieceInfo -> PieceCount
pieceCount PieceInfo {..} = BS.length (unHashList piPieceHashes) `quot` hashsize

-- | Test if this is last piece in torrent content.
isLastPiece :: PieceInfo -> PieceIx -> Bool
isLastPiece ci i = pieceCount ci == succ i

-- | Validate piece with metainfo hash.
checkPieceLazy :: PieceInfo -> Piece BL.ByteString -> Bool
checkPieceLazy pinfo @ PieceInfo {..} Piece {..}
  = (fromIntegral (BL.length pieceData) == piPieceLength
     || isLastPiece pinfo pieceIndex)
  && SHA1.hashlazy pieceData == pieceHash pinfo pieceIndex

{-----------------------------------------------------------------------
--  Info dictionary
-----------------------------------------------------------------------}

{- note that info hash is actually reduntant field
   but it's better to keep it here to avoid heavy recomputations
-}

-- | Info part of the .torrent file contain info about each content file.
data InfoDict = InfoDict
  { idInfoHash     :: !InfoHash
    -- ^ SHA1 hash of the (other) 'DictInfo' fields.

  , idLayoutInfo   :: !LayoutInfo
    -- ^ File layout (name, size, etc) information.

  , idPieceInfo    :: !PieceInfo
    -- ^ Content validation information.

  , idPrivate      :: !Bool
    -- ^ If set the client MUST publish its presence to get other
    -- peers ONLY via the trackers explicity described in the
    -- metainfo file.
    --
    --   BEP 27: <http://www.bittorrent.org/beps/bep_0027.html>
  } deriving (Show, Read, Eq, Typeable)

makeLensesFor
  [ ("idInfoHash"  , "infohash"  )
  , ("idLayoutInfo", "layoutInfo")
  , ("idPieceInfo" , "pieceInfo" )
  , ("idPrivate"   , "isPrivate" )
  ]
  ''InfoDict

instance NFData InfoDict where
  rnf InfoDict {..} = rnf idLayoutInfo

instance Hashable InfoDict where
  hashWithSalt = Hashable.hashUsing idInfoHash
  {-# INLINE hashWithSalt #-}

-- | Empty info dictionary with zero-length content.
instance Default InfoDict where
  def = infoDictionary def def False

-- | Smart constructor: add a info hash to info dictionary.
infoDictionary :: LayoutInfo -> PieceInfo -> Bool -> InfoDict
infoDictionary li pinfo private = InfoDict ih li pinfo private
  where
    ih = hashLazyIH $ BE.encode $ InfoDict def li pinfo private

getPrivate :: BE.Get Bool
getPrivate = (Just True ==) <$>? "private"

putPrivate :: Bool -> BDict -> BDict
putPrivate False = id
putPrivate True  = \ cont -> "private" .=! True .: cont

-- | Hash lazy bytestring using SHA1 algorithm.
hashLazyIH :: BL.ByteString -> InfoHash
hashLazyIH = either (const (error msg)) id . safeConvert . C.hashlazy
  where
    msg = "Infohash.hash: impossible: SHA1 is always 20 bytes long"

instance BEncode InfoDict where
  toBEncode InfoDict {..} = toDict $
      putLayoutInfo idLayoutInfo   $
      putPieceInfo  idPieceInfo    $
      putPrivate    idPrivate      $
      endDict

  fromBEncode dict = (`fromDict` dict) $ do
      InfoDict ih <$> getLayoutInfo
                  <*> getPieceInfo
                  <*> getPrivate
    where
      ih = hashLazyIH (BE.encode dict)

ppPrivacy :: Bool -> Doc
ppPrivacy privacy = "Privacy: " <> if privacy then "private" else "public"

--ppAdditionalInfo :: InfoDict -> Doc
--ppAdditionalInfo layout = PP.empty

instance Pretty InfoDict where
  pretty InfoDict {..} =
    pretty idLayoutInfo $$
    pretty  idPieceInfo  $$
    ppPrivacy    idPrivate

{-----------------------------------------------------------------------
--  Torrent info
-----------------------------------------------------------------------}
-- TODO add torrent file validation

-- | Metainfo about particular torrent.
data Torrent = Torrent
  { tAnnounce     :: !(Maybe URI)
    -- ^ The URL of the tracker.

  , tAnnounceList :: !(Maybe [[URI]])
    -- ^ Announce list add multiple tracker support.
    --
    --   BEP 12: <http://www.bittorrent.org/beps/bep_0012.html>

  , tComment      :: !(Maybe Text)
    -- ^ Free-form comments of the author.

  , tCreatedBy    :: !(Maybe Text)
    -- ^ Name and version of the program used to create the .torrent.

  , tCreationDate :: !(Maybe POSIXTime)
    -- ^ Creation time of the torrent, in standard UNIX epoch.

  , tEncoding     :: !(Maybe Text)
    -- ^ String encoding format used to generate the pieces part of
    --   the info dictionary in the .torrent metafile.

  , tInfoDict     :: !InfoDict
    -- ^ Info about each content file.

  , tNodes        :: !(Maybe [NodeAddr HostName])
    -- ^ This key should be set to the /K closest/ nodes in the
    -- torrent generating client's routing table. Alternatively, the
    -- key could be set to a known good 'Network.BitTorrent.Core.Node'
    -- such as one operated by the person generating the torrent.
    --
    -- Please do not automatically add \"router.bittorrent.com\" to
    -- this list because different bittorrent software may prefer to
    -- use different bootstrap node.

  , tPublisher    :: !(Maybe URI)
    -- ^ Containing the RSA public key of the publisher of the
    -- torrent.  Private counterpart of this key that has the
    -- authority to allow new peers onto the swarm.

  , tPublisherURL :: !(Maybe URI)
  , tSignature    :: !(Maybe ByteString)
    -- ^ The RSA signature of the info dictionary (specifically, the
    --   encrypted SHA-1 hash of the info dictionary).
    } deriving (Show, Eq, Typeable)

makeLensesFor
  [ ("tAnnounce"    , "announce"    )
  , ("tAnnounceList", "announceList")
  , ("tComment"     , "comment"     )
  , ("tCreatedBy"   , "createdBy"   )
  , ("tCreationDate", "creationDate")
  , ("tEncoding"    , "encoding"    )
  , ("tInfoDict"    , "infoDict"    )
  , ("tPublisher"   , "publisher"   )
  , ("tPublisherURL", "publisherURL")
  , ("tSignature"   , "signature"   )
  ]
  ''Torrent

instance NFData Torrent where
  rnf Torrent {..} = rnf tInfoDict

-- TODO move to bencoding
instance BEncode URI where
  toBEncode uri = toBEncode (BC.pack (uriToString id uri ""))
  {-# INLINE toBEncode #-}

  fromBEncode (BString s) | Just url <- parseURI (BC.unpack s) = return url
  fromBEncode b           = decodingError $ "url <" ++ show b ++ ">"
  {-# INLINE fromBEncode #-}

--pico2uni :: Pico -> Uni
--pico2uni = undefined

-- TODO move to bencoding
instance BEncode POSIXTime where
  toBEncode pt = toBEncode (floor pt :: Integer)
  fromBEncode (BInteger i) = return $ fromIntegral i
  fromBEncode _            = decodingError $ "POSIXTime"

-- TODO to bencoding package
instance BEncode String where
  toBEncode = toBEncode . T.pack
  fromBEncode v = T.unpack <$> fromBEncode v

instance BEncode Torrent where
  toBEncode Torrent {..} = toDict $
       "announce"      .=? tAnnounce
    .: "announce-list" .=? tAnnounceList
    .: "comment"       .=? tComment
    .: "created by"    .=? tCreatedBy
    .: "creation date" .=? tCreationDate
    .: "encoding"      .=? tEncoding
    .: "info"          .=! tInfoDict
    .: "nodes"         .=? tNodes
    .: "publisher"     .=? tPublisher
    .: "publisher-url" .=? tPublisherURL
    .: "signature"     .=? tSignature
    .: endDict

  fromBEncode = fromDict $ do
    Torrent <$>? "announce"
            <*>? "announce-list"
            <*>? "comment"
            <*>? "created by"
            <*>? "creation date"
            <*>? "encoding"
            <*>! "info"
            <*>? "nodes"
            <*>? "publisher"
            <*>? "publisher-url"
            <*>? "signature"

(<:>) :: Doc -> Doc -> Doc
name <:>   v       = name <> ":" <+> v

(<:>?) :: Doc -> Maybe Doc -> Doc
_    <:>?  Nothing = PP.empty
name <:>? (Just d) = name <:> d

instance Pretty Torrent where
  pretty Torrent {..} =
       "InfoHash: " <> pretty (idInfoHash tInfoDict)
    $$ hang "General" 4 generalInfo
    $$ hang "Tracker" 4 trackers
    $$ pretty tInfoDict
   where
    trackers = case tAnnounceList of
        Nothing  -> text (show tAnnounce)
        Just xxs -> vcat $ L.map ppTier $ L.zip [1..] xxs
      where
        ppTier (n, xs) = "Tier #" <> int n <:> vcat (L.map (text . show) xs)

    generalInfo =
        "Comment"       <:>? ((text . T.unpack) <$> tComment)      $$
        "Created by"    <:>? ((text . T.unpack) <$> tCreatedBy)    $$
        "Created on"    <:>? ((text . show . posixSecondsToUTCTime)
                               <$> tCreationDate) $$
        "Encoding"      <:>? ((text . T.unpack) <$> tEncoding)     $$
        "Publisher"     <:>? ((text . show) <$> tPublisher)    $$
        "Publisher URL" <:>? ((text . show) <$> tPublisherURL) $$
        "Signature"     <:>? ((text . show) <$> tSignature)

-- | No files, no trackers, no nodes, etc...
instance Default Torrent where
  def = nullTorrent def

-- | A simple torrent contains only required fields.
nullTorrent :: InfoDict -> Torrent
nullTorrent info = Torrent
    Nothing Nothing Nothing Nothing Nothing Nothing
    info    Nothing Nothing Nothing Nothing

-- | Mime type of torrent files.
typeTorrent :: BS.ByteString
typeTorrent = "application/x-bittorrent"

-- | Extension usually used for torrent files.
torrentExt :: String
torrentExt = "torrent"

-- | Test if this path has proper extension.
isTorrentPath :: FilePath -> Bool
isTorrentPath filepath = takeExtension filepath == extSeparator : torrentExt

-- | Read and decode a .torrent file.
fromFile :: FilePath -> IO Torrent
fromFile filepath = do
  contents <- BS.readFile filepath
  case BE.decode contents of
    Right !t -> return t
    Left msg -> throwIO $ userError $ msg ++ " while reading torrent file"

-- | Encode and write a .torrent file.
toFile :: FilePath -> Torrent -> IO ()
toFile filepath = BL.writeFile filepath . BE.encode

{-----------------------------------------------------------------------
--  URN
-----------------------------------------------------------------------}

-- | Namespace identifier determines the syntactic interpretation of
-- namespace-specific string.
type NamespaceId = [Text]

-- | BitTorrent Info Hash (hence the name) namespace
-- identifier. Namespace-specific string /should/ be a base16\/base32
-- encoded SHA1 hash of the corresponding torrent /info/ dictionary.
--
btih :: NamespaceId
btih  = ["btih"]

-- | URN is pesistent location-independent identifier for
--   resources. In particular, URNs are used represent torrent names
--   as a part of magnet link, see 'Data.Torrent.Magnet.Magnet' for
--   more info.
--
data URN = URN
  { urnNamespace :: NamespaceId -- ^ a namespace identifier;
  , urnString    :: Text        -- ^ a corresponding
                                -- namespace-specific string.
  } deriving (Eq, Ord, Typeable)

-----------------------------------------------------------------------

instance Convertible URN InfoHash where
  safeConvert u @ URN {..}
    | urnNamespace /= btih = convError "invalid namespace" u
    |       otherwise      = safeConvert urnString

-- | Make resource name for torrent with corresponding
-- infohash. Infohash is base16 (hex) encoded.
--
infohashURN :: InfoHash -> URN
infohashURN = URN btih . longHex

-- | Meaningless placeholder value.
instance Default URN where
  def = infohashURN def

------------------------------------------------------------------------

-- | Render URN to its text representation.
renderURN :: URN -> Text
renderURN URN {..}
  = T.intercalate ":" $ "urn" : urnNamespace ++ [urnString]

instance Pretty URN where
  pretty = text . T.unpack . renderURN

instance Show URN where
  showsPrec n = showsPrec n . T.unpack . renderURN

instance QueryValueLike URN where
  toQueryValue = toQueryValue . renderURN
  {-# INLINE toQueryValue #-}

-----------------------------------------------------------------------

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (L.init xs, L.last xs)

instance Convertible Text URN where
  safeConvert t = case T.split (== ':') t of
    uriScheme : body
      | T.toLower uriScheme == "urn" ->
        case unsnoc body of
          Just (namespace, val) -> pure URN
            { urnNamespace = namespace
            , urnString    = val
            }
          Nothing -> convError "missing URN string" body
      | otherwise -> convError "invalid URN scheme" uriScheme
    []            -> convError "missing URN scheme" t

instance IsString URN where
  fromString = either (error . prettyConvertError) id
             . safeConvert . T.pack

-- | Try to parse an URN from its text representation.
--
--  Use 'safeConvert' for detailed error messages.
--
parseURN :: Text -> Maybe URN
parseURN = either (const Nothing) pure . safeConvert

{-----------------------------------------------------------------------
--  Magnet
-----------------------------------------------------------------------}
-- $magnet-link
--
--   Magnet URI scheme is an standard defining Magnet links. Magnet
--   links are refer to resources by hash, in particular magnet links
--   can refer to torrent using corresponding infohash. In this way,
--   magnet links can be used instead of torrent files.
--
--   This module provides bittorrent specific implementation of magnet
--   links.
--
--   For generic magnet uri scheme see:
--   <http://magnet-uri.sourceforge.net/magnet-draft-overview.txt>,
--   <http://www.iana.org/assignments/uri-schemes/prov/magnet>
--
--   Bittorrent specific details:
--   <http://www.bittorrent.org/beps/bep_0009.html>
--

-- TODO multiple exact topics
-- TODO render/parse supplement for URI/query

-- | An URI used to identify torrent.
data Magnet = Magnet
  { -- | Torrent infohash hash. Can be used in DHT queries if no
    -- 'tracker' provided.
    exactTopic  :: !InfoHash -- TODO InfoHash -> URN?

    -- | A filename for the file to download. Can be used to
    -- display name while waiting for metadata.
  , displayName :: Maybe Text

    -- | Size of the resource in bytes.
  , exactLength :: Maybe Integer

    -- | URI pointing to manifest, e.g. a list of further items.
  , manifest :: Maybe Text

    -- | Search string.
  , keywordTopic :: Maybe Text

    -- | A source to be queried after not being able to find and
    -- download the file in the bittorrent network in a defined
    -- amount of time.
  , acceptableSource :: Maybe URI

    -- | Direct link to the resource.
  , exactSource      :: Maybe URI

    -- | URI to the tracker.
  , tracker :: Maybe URI

    -- | Additional or experimental parameters.
  , supplement :: Map Text Text
  } deriving (Eq, Ord, Typeable)

instance QueryValueLike Integer where
  toQueryValue = toQueryValue . show

instance QueryValueLike URI where
  toQueryValue = toQueryValue . show

instance QueryLike Magnet where
  toQuery Magnet {..} =
    [ ("xt", toQueryValue $ infohashURN exactTopic)
    , ("dn", toQueryValue displayName)
    , ("xl", toQueryValue exactLength)
    , ("mt", toQueryValue manifest)
    , ("kt", toQueryValue keywordTopic)
    , ("as", toQueryValue acceptableSource)
    , ("xs", toQueryValue exactSource)
    , ("tr", toQueryValue tracker)
    ]

instance QueryValueLike Magnet where
  toQueryValue = toQueryValue . renderMagnet

instance Convertible QueryText Magnet where
  safeConvert xs = do
      urnStr   <- getTextMsg "xt" "exact topic not defined" xs
      infoHash <- convertVia (error "safeConvert" :: URN)  urnStr
      return Magnet
        { exactTopic       = infoHash
        , displayName      = getText "dn" xs
        , exactLength      = getText "xl" xs >>= getInt
        , manifest         = getText "mt" xs
        , keywordTopic     = getText "kt" xs
        , acceptableSource = getText "as" xs >>= getURI
        , exactSource      = getText "xs" xs >>= getURI
        , tracker          = getText "tr" xs >>= getURI
        , supplement       = M.empty
        }
    where
      getInt    = either (const Nothing) (Just . fst) . signed decimal
      getURI    = parseURI . T.unpack
      getText p = join . L.lookup p
      getTextMsg p msg ps = maybe (convError msg xs) pure $ getText p ps

magnetScheme :: URI
magnetScheme = URI
    { uriScheme    = "magnet:"
    , uriAuthority = Nothing
    , uriPath      = ""
    , uriQuery     = ""
    , uriFragment  = ""
    }

isMagnetURI :: URI -> Bool
isMagnetURI u = u { uriQuery = "" } == magnetScheme

-- | Can be used instead of 'parseMagnet'.
instance Convertible URI Magnet where
  safeConvert u @ URI {..}
    | not (isMagnetURI u) = convError "this is not a magnet link" u
    |      otherwise      = safeConvert $ parseQueryText $ BC.pack uriQuery

-- | Can be used instead of 'renderMagnet'.
instance Convertible Magnet URI where
  safeConvert m = pure $ magnetScheme
    { uriQuery = BC.unpack $ renderQuery True $ toQuery m }

instance Convertible String Magnet where
  safeConvert str
    | Just uri <- parseURI str = safeConvert uri
    |        otherwise         = convError "unable to parse uri" str

------------------------------------------------------------------------

-- | Meaningless placeholder value.
instance Default Magnet where
  def = Magnet
    { exactTopic       = def
    , displayName      = Nothing
    , exactLength      = Nothing
    , manifest         = Nothing
    , keywordTopic     = Nothing
    , acceptableSource = Nothing
    , exactSource      = Nothing
    , tracker          = Nothing
    , supplement       = M.empty
    }

-- | Set 'exactTopic' ('xt' param) only, other params are empty.
nullMagnet :: InfoHash -> Magnet
nullMagnet u = Magnet
    { exactTopic   = u
    , displayName  = Nothing
    , exactLength  = Nothing
    , manifest     = Nothing
    , keywordTopic = Nothing
    , acceptableSource = Nothing
    , exactSource      = Nothing
    , tracker    = Nothing
    , supplement = M.empty
    }

-- | Like 'nullMagnet' but also include 'displayName' ('dn' param).
simpleMagnet :: Torrent -> Magnet
simpleMagnet Torrent {tInfoDict = InfoDict {..}}
  = (nullMagnet idInfoHash)
    { displayName = Just $ T.decodeUtf8 $ suggestedName idLayoutInfo
    }

-- | Like 'simpleMagnet' but also include 'exactLength' ('xl' param) and
-- 'tracker' ('tr' param).
--
detailedMagnet :: Torrent -> Magnet
detailedMagnet t @ Torrent {tInfoDict = InfoDict {..}, tAnnounce}
  = (simpleMagnet t)
    { exactLength = Just $ fromIntegral $ contentLength idLayoutInfo
    , tracker     = tAnnounce
    }

-----------------------------------------------------------------------

parseMagnetStr :: String -> Maybe Magnet
parseMagnetStr = either (const Nothing) Just . safeConvert

renderMagnetStr :: Magnet -> String
renderMagnetStr = show . (convert :: Magnet -> URI)

instance Pretty Magnet where
  pretty = PP.text . renderMagnetStr

instance Show Magnet where
  show = renderMagnetStr
  {-# INLINE show #-}

instance Read Magnet where
  readsPrec _ xs
      | Just m <- parseMagnetStr mstr = [(m, rest)]
      | otherwise = []
    where
      (mstr, rest) = L.break (== ' ') xs

instance IsString Magnet where
  fromString str = fromMaybe (error msg) $ parseMagnetStr str
    where
      msg = "unable to parse magnet: " ++ str

-- | Try to parse magnet link from urlencoded string. Use
-- 'safeConvert' to find out error location.
--
parseMagnet :: Text -> Maybe Magnet
parseMagnet = parseMagnetStr . T.unpack
{-# INLINE parseMagnet #-}

-- | Render magnet link to urlencoded string
renderMagnet :: Magnet -> Text
renderMagnet = T.pack . renderMagnetStr
{-# INLINE renderMagnet #-}
