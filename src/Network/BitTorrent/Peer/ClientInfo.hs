-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--  This module detect client information such as version and
--  implementation that can be later printed in human frienly
--  form. Useful for debugging and logging.
--
--  See <http://bittorrent.org/beps/bep_0020.html> for more
--  information.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Peer.ClientInfo
       ( -- * Info
         ClientInfo(..), clientInfo, ppClientInfo, unknownClient

         -- * Version
       , ClientVersion, ppClientVersion

         -- * Implementation
       , ClientImpl(..), ppClientImpl

--       , mkEnumTyDef, mkPars, nameMap
       ) where

import Control.Applicative
--import Data.List
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Serialize.Get
import Text.PrettyPrint

import Network.BitTorrent.Peer.ID


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
   deriving (Show, Eq, Ord)

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

-- | Format client implementation info in human readable form.
ppClientImpl :: ClientImpl -> Doc
ppClientImpl = text . tail . show

unknownImpl :: ClientImpl
unknownImpl = IUnknown



type ClientVersion = ByteString

-- | Format client implementation version in human readable form.
ppClientVersion :: ClientVersion -> Doc
ppClientVersion = text . BC.unpack

unknownVersion :: ClientVersion
unknownVersion = "0000"


-- | All useful infomation that can be obtained from a peer
-- identifier.
data ClientInfo = ClientInfo {
    ciImpl    :: ClientImpl
  , ciVersion :: ClientVersion
  } deriving (Show, Eq, Ord)

-- | Format client implementation in human readable form.
ppClientInfo :: ClientInfo -> Doc
ppClientInfo ClientInfo {..} =
  ppClientImpl ciImpl <+> "version" <+> ppClientVersion ciVersion


-- | Unrecognized client implementation.
unknownClient :: ClientInfo
unknownClient = ClientInfo unknownImpl unknownVersion

-- | Tries to extract meaningful information from peer ID bytes. If
--   peer id uses unknown coding style then client info returned is
--   'unknownClient'.
--
clientInfo :: PeerID -> ClientInfo
clientInfo pid = either (const unknownClient) id $ runGet getCI (getPeerID pid)
  where -- TODO other styles
    getCI = do
      _ <- getWord8
      ClientInfo <$> (parseImpl <$> getByteString 2) <*> getByteString 4


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