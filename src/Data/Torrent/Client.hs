-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   'ClientInfo' is used to identify the client implementation and
--   version which also contained in 'Peer'. For exsample first 6
--   bytes of peer id of this this library are @-HS0100-@ while for
--   mainline we have @M4-3-6--@.  We could extract this info and
--   print in human-friendly form: this is useful for debugging and
--   logging.
--
--   For more information see:
--   <http://bittorrent.org/beps/bep_0020.html>
--
--
--   NOTE: Do /not/ use this information to control client
--   capabilities (such as supported enchancements), this should be
--   done using 'Network.BitTorrent.Extension'!
--
{-# OPTIONS -fno-warn-orphans #-}
module Data.Torrent.Client
       ( ClientImpl (..)
       , ClientInfo (..)
       , libClientInfo
       ) where

import Data.Default
import Data.List as L
import Data.List.Split as L
import Data.String
import Data.Version
import Text.PrettyPrint hiding ((<>))
import Text.PrettyPrint.Class
import Text.Read (readMaybe)
-- import Paths_bittorrent (version)

-- TODO FIXME
version :: Version
version = Version [0, 0, 0, 3] []

-- | List of registered client versions + 'IlibHSbittorrent' (this
-- package) + 'IUnknown' (for not recognized software). All names are
-- prefixed by \"I\" because some of them starts from lowercase letter
-- but that is not a valid Haskell constructor name.
--
data ClientImpl =
   IUnknown

 | IMainline

 | IABC
 | IOspreyPermaseed
 | IBTQueue
 | ITribler
 | IShadow
 | IBitTornado

-- UPnP(!) Bit Torrent !???
-- 'U' - UPnP NAT Bit Torrent
 | IBitLord
 | IOpera
 | IMLdonkey

 | IAres
 | IArctic
 | IAvicora
 | IBitPump
 | IAzureus
 | IBitBuddy
 | IBitComet
 | IBitflu
 | IBTG
 | IBitRocket
 | IBTSlave
 | IBittorrentX
 | IEnhancedCTorrent
 | ICTorrent
 | IDelugeTorrent
 | IPropagateDataClient
 | IEBit
 | IElectricSheep
 | IFoxTorrent
 | IGSTorrent
 | IHalite
 | IlibHSbittorrent
 | IHydranode
 | IKGet
 | IKTorrent
 | ILH_ABC
 | ILphant
 | ILibtorrent
 | ILibTorrent
 | ILimeWire
 | IMonoTorrent
 | IMooPolice
 | IMiro
 | IMoonlightTorrent
 | INetTransport
 | IPando
 | IqBittorrent
 | IQQDownload
 | IQt4TorrentExample
 | IRetriever
 | IShareaza
 | ISwiftbit
 | ISwarmScope
 | ISymTorrent
 | Isharktorrent
 | ITorrentDotNET
 | ITransmission
 | ITorrentstorm
 | ITuoTu
 | IuLeecher
 | IuTorrent
 | IVagaa
 | IBitLet
 | IFireTorrent
 | IXunlei
 | IXanTorrent
 | IXtorrent
 | IZipTorrent
   deriving (Show, Eq, Ord, Enum, Bounded)

-- | Used to represent a not recognized implementation
instance Default ClientImpl where
  def = IUnknown
  {-# INLINE def #-}

-- | Example: @\"BitLet\" == 'IBitLet'@
instance IsString ClientImpl where
  fromString str
    | Just impl <- L.lookup str alist = impl
    | otherwise = error $ "fromString: not recognized " ++ str
    where
      alist = L.map mk [minBound..maxBound]
      mk  x = (L.tail $ show x, x)

-- | Example: @pretty 'IBitLet' == \"IBitLet\"@
instance Pretty ClientImpl where
  pretty = text . L.tail . show

-- | Just the '0' version.
instance Default Version where
  def = Version [0] []
  {-# INLINE def #-}

-- | For dot delimited version strings.
--   Example: @fromString \"0.1.0.2\" == Version [0, 1, 0, 2]@
--
instance IsString Version where
  fromString str
    | Just nums <- chunkNums str = Version nums []
    | otherwise = error $ "fromString: invalid version string " ++ str
    where
      chunkNums = sequence . L.map readMaybe . L.linesBy ('.' ==)

instance Pretty Version where
  pretty = text . showVersion

-- | The all sensible infomation that can be obtained from a peer
-- identifier or torrent /createdBy/ field.
data ClientInfo = ClientInfo {
    ciImpl    :: ClientImpl
  , ciVersion :: Version
  } deriving (Show, Eq, Ord)

-- | Unrecognized client implementation.
instance Default ClientInfo where
  def = ClientInfo def def
  {-# INLINE def #-}

-- | Example: @\"BitComet-1.2\" == ClientInfo IBitComet (Version [1, 2] [])@
instance IsString ClientInfo where
  fromString str
    | _ : ver <- _ver = ClientInfo (fromString impl) (fromString ver)
    | otherwise = error $ "fromString: invalid client info string" ++ str
    where
      (impl, _ver) = L.span ((/=) '-') str

instance Pretty ClientInfo where
  pretty ClientInfo {..} = pretty ciImpl <+> "version" <+> pretty ciVersion

-- | Client info of this (the bittorrent library) package. Normally,
-- applications should introduce its own idenitifiers, otherwise they
-- can use 'libClientInfo' value.
--
libClientInfo :: ClientInfo
libClientInfo = ClientInfo IlibHSbittorrent version

{-----------------------------------------------------------------------
--  For torrent file
-----------------------------------------------------------------------}
-- TODO collect information about createdBy torrent field
{-
renderImpl :: ClientImpl -> Text
renderImpl = T.pack . L.tail . show

renderVersion :: Version -> Text
renderVersion = undefined

renderClientInfo :: ClientInfo -> Text
renderClientInfo ClientInfo {..} = renderImpl ciImpl <> "/" <> renderVersion ciVersion

parseClientInfo :: Text -> ClientImpl
parseClientInfo t = undefined
-}
{-
-- code used for generation; remove it later on

mkEnumTyDef :: NM -> String
mkEnumTyDef = unlines . map (" | I" ++) . nub . map snd

mkPars :: NM -> String
mkPars = unlines . map (\(code, impl) -> "  f \"" ++ code ++ "\" = " ++ "I" ++ impl)

type NM = [(String, String)]
nameMap :: NM
nameMap =
 [ ("AG", "Ares")
 , ("A~", "Ares")
 , ("AR", "Arctic")
 , ("AV", "Avicora")
 , ("AX", "BitPump")
 , ("AZ", "Azureus")
 , ("BB", "BitBuddy")
 , ("BC", "BitComet")
 , ("BF", "Bitflu")
 , ("BG", "BTG")
 , ("BR", "BitRocket")
 , ("BS", "BTSlave")
 , ("BX", "BittorrentX")
 , ("CD", "EnhancedCTorrent")
 , ("CT", "CTorrent")
 , ("DE", "DelugeTorrent")
 , ("DP", "PropagateDataClient")
 , ("EB", "EBit")
 , ("ES", "ElectricSheep")
 , ("FT", "FoxTorrent")
 , ("GS", "GSTorrent")
 , ("HL", "Halite")
 , ("HS", "libHSnetwork_bittorrent")
 , ("HN", "Hydranode")
 , ("KG", "KGet")
 , ("KT", "KTorrent")
 , ("LH", "LH_ABC")
 , ("LP", "Lphant")
 , ("LT", "Libtorrent")
 , ("lt", "LibTorrent")
 , ("LW", "LimeWire")
 , ("MO", "MonoTorrent")
 , ("MP", "MooPolice")
 , ("MR", "Miro")
 , ("MT", "MoonlightTorrent")
 , ("NX", "NetTransport")
 , ("PD", "Pando")
 , ("qB", "qBittorrent")
 , ("QD", "QQDownload")
 , ("QT", "Qt4TorrentExample")
 , ("RT", "Retriever")
 , ("S~", "Shareaza")
 , ("SB", "Swiftbit")
 , ("SS", "SwarmScope")
 , ("ST", "SymTorrent")
 , ("st", "sharktorrent")
 , ("SZ", "Shareaza")
 , ("TN", "TorrentDotNET")
 , ("TR", "Transmission")
 , ("TS", "Torrentstorm")
 , ("TT", "TuoTu")
 , ("UL", "uLeecher")
 , ("UT", "uTorrent")
 , ("VG", "Vagaa")
 , ("WT", "BitLet")
 , ("WY", "FireTorrent")
 , ("XL", "Xunlei")
 , ("XT", "XanTorrent")
 , ("XX", "Xtorrent")
 , ("ZT", "ZipTorrent")
 ]
-}
