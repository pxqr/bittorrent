-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--  'PeerID' represent self assigned peer identificator. Ideally each
--  host in the network should have unique peer id to avoid
--  collisions, therefore for peer ID generation we use good entropy
--  source. Peer ID is sent in /tracker request/, sent and received in
--  /peer handshakes/ and used in DHT queries.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BitTorrent.Core.PeerId
       ( -- * PeerId
         PeerId (getPeerId)
       , byteStringToPeerId

         -- * Generation
       , genPeerId
       , timestamp
       , entropy

         -- * Encoding
       , azureusStyle
       , shadowStyle
       , defaultClientId
       , defaultVersionNumber

         -- * Decoding
       , clientInfo
       ) where

import Control.Applicative
import Data.Aeson
import Data.BEncode as BE
import Data.ByteString as BS
import Data.ByteString.Internal as BS
import Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BS
import Data.Default
import Data.Foldable    (foldMap)
import Data.List as L
import Data.List.Split as L
import Data.Maybe       (fromMaybe, catMaybes)
import Data.Monoid
import Data.Serialize as S
import Data.String
import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (formatTime)
import Data.URLEncoded
import Data.Version     (Version(Version), versionBranch)
import System.Entropy   (getEntropy)
import System.Locale    (defaultTimeLocale)
import Text.PrettyPrint hiding ((<>))
import Text.PrettyPrint.Class
import Text.Read        (readMaybe)
import Paths_bittorrent (version)

import Data.Torrent.Client

-- TODO use unpacked Word160 form (length is known statically)

-- | Peer identifier is exactly 20 bytes long bytestring.
newtype PeerId = PeerId { getPeerId :: ByteString }
                 deriving (Show, Eq, Ord, BEncode, ToJSON, FromJSON)

peerIdLen :: Int
peerIdLen = 20

instance Serialize PeerId where
  put = putByteString . getPeerId
  get = PeerId <$> getBytes peerIdLen

instance URLShow PeerId where
  urlShow = BC.unpack . getPeerId

instance IsString PeerId where
  fromString str
      | BS.length bs == peerIdLen = PeerId bs
      | otherwise = error $ "Peer id should be 20 bytes long: " ++ show str
    where
      bs = fromString str

instance Pretty PeerId where
  pretty = text . BC.unpack . getPeerId

byteStringToPeerId :: BS.ByteString -> Maybe PeerId
byteStringToPeerId bs
  | BS.length bs == peerIdLen = Just (PeerId bs)
  |          otherwise        = Nothing

{-----------------------------------------------------------------------
--  Encoding
-----------------------------------------------------------------------}

-- | Pad bytestring so it's becomes exactly request length. Conversion
-- is done like so:
--
--     * length < size: Complete bytestring by given charaters.
--
--     * length = size: Output bytestring as is.
--
--     * length > size: Drop last (length - size) charaters from a
--     given bytestring.
--
byteStringPadded :: ByteString -- ^ bytestring to be padded.
                 -> Int        -- ^ size of result builder.
                 -> Char       -- ^ character used for padding.
                 -> BS.Builder
byteStringPadded bs s c =
      BS.byteString (BS.take s bs) <>
      BS.byteString (BC.replicate padLen c)
  where
    padLen = s - min (BS.length bs) s

-- | Azureus-style encoding have the following layout:
--
--     * 1  byte : '-'
--
--     * 2  bytes: client id
--
--     * 4  bytes: version number
--
--     * 1  byte : '-'
--
--     * 12 bytes: random number
--
azureusStyle :: ByteString -- ^ 2 character client ID, padded with 'H'.
             -> ByteString -- ^ Version number, padded with 'X'.
             -> ByteString -- ^ Random number, padded with '0'.
             -> PeerId     -- ^ Azureus-style encoded peer ID.
azureusStyle cid ver rnd = PeerId $ BL.toStrict $ BS.toLazyByteString $
    BS.char8 '-' <>
      byteStringPadded cid 2  'H' <>
      byteStringPadded ver 4  'X' <>
    BS.char8 '-' <>
      byteStringPadded rnd 12 '0'

-- | Shadow-style encoding have the following layout:
--
--     * 1 byte   : client id.
--
--     * 0-4 bytes: version number. If less than 4 then padded with
--     '-' char.
--
--     * 15 bytes : random number. If length is less than 15 then
--     padded with '0' char.
--
shadowStyle :: Char       -- ^ Client ID.
            -> ByteString -- ^ Version number.
            -> ByteString -- ^ Random number.
            -> PeerId     -- ^ Shadow style encoded peer ID.
shadowStyle cid ver rnd = PeerId $ BL.toStrict $ BS.toLazyByteString $
    BS.char8 cid <>
      byteStringPadded ver 4  '-' <>
      byteStringPadded rnd 15 '0'


-- | 'HS'- 2 bytes long client identifier.
defaultClientId :: ByteString
defaultClientId = "HS"

-- | Gives exactly 4 bytes long version number for any version of the
-- package.  Version is taken from .cabal file.
defaultVersionNumber :: ByteString
defaultVersionNumber = BS.take 4 $ BC.pack $ foldMap show $
                         versionBranch version

{-----------------------------------------------------------------------
--  Generation
-----------------------------------------------------------------------}

-- | Gives 15 characters long decimal timestamp such that:
--
--     * 6 bytes   : first 6 characters from picoseconds obtained with %q.
--
--     * 1 byte    : character \'.\' for readability.
--
--     * 9..* bytes: number of whole seconds since the Unix epoch
--     (!)REVERSED.
--
--   Can be used both with shadow and azureus style encoding. This
--   format is used to make the ID's readable for debugging purposes.
--
timestamp :: IO ByteString
timestamp = (BC.pack . format) <$> getCurrentTime
  where
    format t = L.take 6 (formatTime defaultTimeLocale "%q" t) ++ "." ++
               L.take 9 (L.reverse (formatTime defaultTimeLocale "%s" t))

-- | Gives 15 character long random bytestring. This is more robust
-- method for generation of random part of peer ID than 'timestamp'.
entropy :: IO ByteString
entropy = getEntropy 15

-- NOTE: entropy generates incorrrect peer id

-- |  Here we use 'azureusStyle' encoding with the following args:
--
--      * 'HS' for the client id; ('defaultClientId')
--
--      * Version of the package for the version number;
--      ('defaultVersionNumber')
--
--      * UTC time day ++ day time for the random number. ('timestamp')
--
genPeerId :: IO PeerId
genPeerId = azureusStyle defaultClientId defaultVersionNumber <$> timestamp

{-----------------------------------------------------------------------
--  Decoding
-----------------------------------------------------------------------}

parseImpl :: ByteString -> ClientImpl
parseImpl = f . BC.unpack
 where
  f "AG" = IAres
  f "A~" = IAres
  f "AR" = IArctic
  f "AV" = IAvicora
  f "AX" = IBitPump
  f "AZ" = IAzureus
  f "BB" = IBitBuddy
  f "BC" = IBitComet
  f "BF" = IBitflu
  f "BG" = IBTG
  f "BR" = IBitRocket
  f "BS" = IBTSlave
  f "BX" = IBittorrentX
  f "CD" = IEnhancedCTorrent
  f "CT" = ICTorrent
  f "DE" = IDelugeTorrent
  f "DP" = IPropagateDataClient
  f "EB" = IEBit
  f "ES" = IElectricSheep
  f "FT" = IFoxTorrent
  f "GS" = IGSTorrent
  f "HL" = IHalite
  f "HS" = IlibHSbittorrent
  f "HN" = IHydranode
  f "KG" = IKGet
  f "KT" = IKTorrent
  f "LH" = ILH_ABC
  f "LP" = ILphant
  f "LT" = ILibtorrent
  f "lt" = ILibTorrent
  f "LW" = ILimeWire
  f "MO" = IMonoTorrent
  f "MP" = IMooPolice
  f "MR" = IMiro
  f "ML" = IMLdonkey
  f "MT" = IMoonlightTorrent
  f "NX" = INetTransport
  f "PD" = IPando
  f "qB" = IqBittorrent
  f "QD" = IQQDownload
  f "QT" = IQt4TorrentExample
  f "RT" = IRetriever
  f "S~" = IShareaza
  f "SB" = ISwiftbit
  f "SS" = ISwarmScope
  f "ST" = ISymTorrent
  f "st" = Isharktorrent
  f "SZ" = IShareaza
  f "TN" = ITorrentDotNET
  f "TR" = ITransmission
  f "TS" = ITorrentstorm
  f "TT" = ITuoTu
  f "UL" = IuLeecher
  f "UT" = IuTorrent
  f "VG" = IVagaa
  f "WT" = IBitLet
  f "WY" = IFireTorrent
  f "XL" = IXunlei
  f "XT" = IXanTorrent
  f "XX" = IXtorrent
  f "ZT" = IZipTorrent
  f _    = IUnknown

-- TODO use regexps

-- | Tries to extract meaningful information from peer ID bytes. If
-- peer id uses unknown coding style then client info returned is
-- 'def'.
--
clientInfo :: PeerId -> ClientInfo
clientInfo pid = either (const def) id $ runGet getCI (getPeerId pid)
  where
    getCI    = do
      leading <- BS.w2c <$> getWord8
      case leading of
        '-' -> ClientInfo <$> getAzureusImpl <*> getAzureusVersion
        'M' -> ClientInfo <$> pure IMainline <*> getMainlineVersion
        'e' -> ClientInfo <$> getBitCometImpl <*> getBitCometVersion
        'F' -> ClientInfo <$> getBitCometImpl <*> getBitCometVersion
        c   -> do
          c1 <- w2c <$> lookAhead getWord8
          if c1 == 'P'
            then do
                 _ <- getWord8
                 ClientInfo <$> pure IOpera            <*> getOperaVersion
            else ClientInfo <$> pure (getShadowImpl c) <*> getShadowVersion

    getMainlineVersion = do
      str <- BC.unpack <$> getByteString 7
      let mnums = L.filter (not . L.null) $ L.linesBy ('-' ==) str
      return $ Version (fromMaybe [] $ sequence $ L.map readMaybe mnums) []

    getAzureusImpl    = parseImpl <$> getByteString 2
    getAzureusVersion = mkVer     <$> getByteString 4
      where
        mkVer bs = Version [fromMaybe 0 $ readMaybe $ BC.unpack bs] []

    getBitCometImpl = do
      bs <- getByteString 3
      lookAhead $ do
        _  <- getByteString 2
        lr <- getByteString 4
        return $
          if lr == "LORD" then IBitLord  else
          if bs == "UTB"  then IBitComet else
          if bs == "xbc"  then IBitComet else def

    getBitCometVersion = do
      x <- getWord8
      y <- getWord8
      return $ Version [fromIntegral x, fromIntegral y] []

    getOperaVersion = do
      str <- BC.unpack <$> getByteString 4
      return $ Version [fromMaybe 0 $ readMaybe str] []

    getShadowImpl 'A' = IABC
    getShadowImpl 'O' = IOspreyPermaseed
    getShadowImpl 'Q' = IBTQueue
    getShadowImpl 'R' = ITribler
    getShadowImpl 'S' = IShadow
    getShadowImpl 'T' = IBitTornado
    getShadowImpl  _  = IUnknown

    decodeShadowVerNr :: Char -> Maybe Int
    decodeShadowVerNr c
      | '0' < c && c <= '9' = Just  (fromEnum c - fromEnum '0')
      | 'A' < c && c <= 'Z' = Just ((fromEnum c - fromEnum 'A') + 10)
      | 'a' < c && c <= 'z' = Just ((fromEnum c - fromEnum 'a') + 36)
      |        otherwise    = Nothing

    getShadowVersion = do
      str <- BC.unpack <$> getByteString 5
      return $ Version (catMaybes $ L.map decodeShadowVerNr str) []
