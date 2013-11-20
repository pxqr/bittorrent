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
--   print in human frienly form: this is useful for debugging and
--   logging. For more information see:
--   <http://bittorrent.org/beps/bep_0020.html> NOTE: Do _not_ use
--   this information to control client capabilities (such as
--   supported enchancements), this should be done using
--   'Network.BitTorrent.Extension'!
--
module Data.Torrent.Client
       ( ClientImpl (..)
       , ppClientImpl

       , ClientVersion (..)
       , ppClientVersion

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


-- | All known client versions.
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

-- | Used to represent not recognized implementation
instance Default ClientImpl where
  def = IUnknown

-- | Format client implementation info in human readable form.
ppClientImpl :: ClientImpl -> Doc
ppClientImpl = text . L.tail . show

-- | Raw version of client, normally extracted from peer id.
newtype ClientVersion = ClientVersion { getClientVersion :: Version }
  deriving (Show, Eq, Ord)

instance Default ClientVersion where
  def = ClientVersion $ Version [0] []

-- | Format client implementation version in human readable form.
ppClientVersion :: ClientVersion -> Doc
ppClientVersion = text . showVersion . getClientVersion

-- | All useful infomation that can be obtained from a peer
-- identifier.
data ClientInfo = ClientInfo {
    ciImpl    :: ClientImpl
  , ciVersion :: ClientVersion
  } deriving (Show, Eq, Ord)

-- | Unrecognized client implementation.
instance Default ClientInfo where
  def = ClientInfo def def

-- | Format client implementation in human readable form.
ppClientInfo :: ClientInfo -> Doc
ppClientInfo ClientInfo {..} =
  ppClientImpl ciImpl <+> "version" <+> ppClientVersion ciVersion

libClientInfo :: ClientInfo
libClientInfo = ClientInfo IlibHSbittorrent (ClientVersion version)

{-----------------------------------------------------------------------
--  For torrent file
-----------------------------------------------------------------------}

renderImpl :: ClientImpl -> Text
renderImpl = T.pack . L.tail . show

renderVersion :: ClientVersion -> Text
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
