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
--   NOTE: Do _not_ use this information to control client
--   capabilities (such as supported enchancements), this should be
--   done using 'Network.BitTorrent.Extension'!
--
module Data.Torrent.Client
       ( ClientImpl (..)
       , ppClientImpl
       , ppVersion
       , ClientInfo (..)
       , ppClientInfo
       , libClientInfo
       ) where

import Control.Applicative
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.Default
import Data.List as L
import Data.Monoid
import Data.Text as T
import Data.Version
import Text.PrettyPrint hiding ((<>))
import Paths_bittorrent (version)


-- | List of registered client versions + IlibHSbittorrent (this
-- package) + Unknown (for not recognized software). All names are
-- prefixed by "I" because some of them starts from lowercase letter
-- but that is not a valid Haskell constructor name.
--
data ClientImpl =
   IUnknown
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

-- | Format client implementation info in human-readable form.
ppClientImpl :: ClientImpl -> Doc
ppClientImpl = text . L.tail . show

-- | Just the '0' version.
instance Default Version where
  def = Version [0] []

-- | Format client implementation version in human-readable form.
ppVersion :: Version -> Doc
ppVersion = text . showVersion

-- | The all sensible infomation that can be obtained from a peer
-- identifier or torrent /createdBy/ field.
data ClientInfo = ClientInfo {
    ciImpl    :: ClientImpl
  , ciVersion :: Version
  } deriving (Show, Eq, Ord)

-- | Unrecognized client implementation.
instance Default ClientInfo where
  def = ClientInfo def def

-- | Format client info in human-readable form.
ppClientInfo :: ClientInfo -> Doc
ppClientInfo ClientInfo {..} =
  ppClientImpl ciImpl <+> "version" <+> ppVersion ciVersion

-- | Client info of this (the bittorrent library) package. Normally,
-- applications should introduce its own idenitifiers, otherwise they
-- can use 'libClientInfo' value.
--
libClientInfo :: ClientInfo
libClientInfo = ClientInfo IlibHSbittorrent version

{-----------------------------------------------------------------------
--  For torrent file
-----------------------------------------------------------------------}

renderImpl :: ClientImpl -> Text
renderImpl = T.pack . L.tail . show

renderVersion :: Version -> Text
renderVersion = undefined

renderClientInfo :: ClientInfo -> Text
renderClientInfo ClientInfo {..} = renderImpl ciImpl <> "/" <> renderVersion ciVersion

parseClientInfo :: Text -> ClientImpl
parseClientInfo t = undefined

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
