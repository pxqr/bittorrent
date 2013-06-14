-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
--   This modules provides three datatypes related to a peer as a host:
--
--   * 'PeerID' represent self assigned peer identificator. Ideally
--     each host in the network should have unique peer id to avoid
--     collisions, therefor for peer ID generation we use good entropy
--     source. (FIX not really)  Peer ID is sent in /tracker request/,
--     sent and received in /peer handshakes/ and used in /distributed
--     hash table/ queries.
--
--   * 'PeerAddr' is used to represent peer location. Currently it's
--   just peer IP and peer port but this might be changed later.
--
--   * 'ClientInfo' is used to identify the client implementation and
--     version which also contained in 'Peer'. For exsample first
--     6 bytes of peer id of this this library are @-HS0100-@ while
--     for mainline we have @M4-3-6--@.
--     We could extract this info and print in human frienly form: this
--     is useful for debugging and logging. For more information see:
--     <http://bittorrent.org/beps/bep_0020.html>
--     NOTE: Do _not_ use this information to control client
--     capabilities (such as supported enchancements), this should be
--     done using 'Network.BitTorrent.Extension'!
--
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS  -fno-warn-orphans #-}
module Network.BitTorrent.Peer
       ( -- * Peer identificators
         PeerID (getPeerID), ppPeerID

         -- ** Encoding styles
       , azureusStyle, shadowStyle

         -- ** Defaults
       , defaultClientID, defaultVersionNumber

         -- ** Generation
       , newPeerID, timestampByteString

         -- ** Extra
       , byteStringPadded

         -- * Peer address
       , PeerAddr(..)
       , peerSockAddr
       , connectToPeer, forkListener
       , ppPeer

         -- * Client version detection
         -- ** Info
       , ClientInfo(..), clientInfo, ppClientInfo, unknownClient

         -- ** Version
       , ClientVersion, ppClientVersion

         -- ** Implementation
       , ClientImpl(..), ppClientImpl

       ) where


import Control.Applicative
import Control.Concurrent
import Control.Exception
import Data.BEncode
import Data.Bits
import Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import Data.Foldable    (foldMap)
import Data.Monoid      ((<>))
import Data.Serialize
import Data.URLEncoded
import Data.Version     (Version(Version), versionBranch)
import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (formatTime)
import Text.PrettyPrint (text, Doc, (<+>))
import System.Locale    (defaultTimeLocale)

import Network hiding (accept)
import Network.Socket



-- TODO we have linker error here, so manual hardcoded version for a
-- while.
-- import Paths_network_bittorrent (version)
version :: Version
version = Version [0, 10, 0, 0] []

{-----------------------------------------------------------------------
    Peer identification
-----------------------------------------------------------------------}

-- | Peer identifier is exactly 20 bytes long bytestring.
newtype PeerID = PeerID { getPeerID :: ByteString }
                 deriving (Show, Eq, Ord, BEncodable)

instance Serialize PeerID where
  put = putByteString . getPeerID
  get = PeerID <$> getBytes 20

instance URLShow PeerID where
  urlShow = BC.unpack . getPeerID

-- | Format peer id in human readable form.
ppPeerID :: PeerID -> Doc
ppPeerID = text . BC.unpack . getPeerID


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
             -> PeerID     -- ^ Azureus-style encoded peer ID.
azureusStyle cid ver rnd = PeerID $ BL.toStrict $ B.toLazyByteString $
    B.char8 '-' <>
      byteStringPadded cid 2  'H' <>
      byteStringPadded ver 4  'X' <>
    B.char8 '-' <>
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
            -> PeerID     -- ^ Shadow style encoded peer ID.
shadowStyle cid ver rnd = PeerID $ BL.toStrict $ B.toLazyByteString $
    B.char8 cid <>
      byteStringPadded ver 4  '-' <>
      byteStringPadded rnd 15 '0'


-- | "HS" - 2 bytes long client identifier.
defaultClientID :: ByteString
defaultClientID = "HS"

-- | Gives exactly 4 bytes long version number for any version of the
-- package.  Version is taken from .cabal.
defaultVersionNumber :: ByteString
defaultVersionNumber = B.take 4 $ BC.pack $ foldMap show $
                         versionBranch version

-- | Gives 15 characters long decimal timestamp such that:
--
--     * 6 bytes   : first 6 characters from picoseconds obtained with %q.
--
--     * 1 bytes   : character '.' for readability.
--
--     * 9..* bytes: number of whole seconds since the Unix epoch
--     (!)REVERSED.
--
--   Can be used both with shadow and azureus style encoding. This
--   format is used to make the ID's readable(for debugging) and more
--   or less random.
--
timestampByteString :: IO ByteString
timestampByteString = (BC.pack . format) <$> getCurrentTime
  where
    format t = take 6 (formatTime defaultTimeLocale "%q" t) ++ "." ++
               take 9 (reverse (formatTime defaultTimeLocale "%s" t))

-- |  Here we use Azureus-style encoding with the following args:
--
--      * 'HS' for the client id.
--
--      * Version of the package for the version number
--
--      * UTC time day ++ day time for the random number.
--
newPeerID :: IO PeerID
newPeerID = azureusStyle defaultClientID defaultVersionNumber
                                     <$> timestampByteString

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
                 -> B.Builder
byteStringPadded bs s c =
      B.byteString (B.take s bs) <>
      B.byteString (BC.replicate padLen c)
  where
    padLen = s - min (B.length bs) s


{-----------------------------------------------------------------------
    Client detection
-----------------------------------------------------------------------}

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

-- | Used to represent not recognized implementation
unknownImpl :: ClientImpl
unknownImpl = IUnknown

-- TODO use Data.Version

-- | Raw version of client, normally extracted from peer id.
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
-- peer id uses unknown coding style then client info returned is
-- 'unknownClient'.
--
clientInfo :: PeerID -> ClientInfo
clientInfo pid = either (const unknownClient) id $
                     runGet getCI (getPeerID pid)
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

{-----------------------------------------------------------------------
    Peer address
-----------------------------------------------------------------------}

-- | Peer address info normally extracted from peer list or peer
-- compact list encoding.
data PeerAddr = PeerAddr {
      peerID   :: Maybe PeerID
    , peerIP   :: HostAddress
    , peerPort :: PortNumber
    } deriving (Show, Eq, Ord)

-- TODO check semantic of ord and eq instances

instance BEncodable PortNumber where
  toBEncode = toBEncode . fromEnum
  fromBEncode b = toEnum <$> fromBEncode b

instance BEncodable PeerAddr where
  toBEncode (PeerAddr pid pip pport) = fromAssocs
    [ "peer id" -->? pid
    , "ip"      -->  pip
    , "port"    -->  pport
    ]

  fromBEncode (BDict d) =
    PeerAddr <$> d >--? "peer id"
             <*> d >--  "ip"
             <*> d >--  "port"

  fromBEncode _ = decodingError "PeerAddr"


-- TODO make platform independent, clarify htonl

-- | Convert peer info from tracker response to socket address.  Used
--   for establish connection between peers.
--
peerSockAddr :: PeerAddr -> SockAddr
peerSockAddr = SockAddrInet <$> (g . peerPort) <*> (htonl . peerIP)
  where
    htonl :: Word32 -> Word32
    htonl d =
       ((d .&. 0xff) `shiftL` 24) .|.
       (((d `shiftR` 8 ) .&. 0xff) `shiftL` 16) .|.
       (((d `shiftR` 16) .&. 0xff) `shiftL` 8)  .|.
       ((d `shiftR` 24) .&. 0xff)

    g :: PortNumber -> PortNumber
    g = id

-- | Tries to connect to peer using reasonable default parameters.
connectToPeer :: PeerAddr -> IO Socket
connectToPeer p = do
  sock <- socket AF_INET Stream Network.Socket.defaultProtocol
  connect sock (peerSockAddr p)
  return sock


forkListener :: ((PeerAddr, Socket) -> IO ()) -> IO PortNumber
forkListener action = do
    sock <- socket AF_INET Stream defaultProtocol
    bindSocket sock (SockAddrInet 0 0)
    listen sock 1
    addr <- getSocketName sock
    case addr of
      SockAddrInet port _ -> do
        forkIO (loop sock)
        return port
      _                   -> do
        throwIO $ userError "listener: impossible happened"
  where
    loop sock = do
      (conn, addr) <- accept sock
      case addr of
        SockAddrInet port host ->
          action (PeerAddr Nothing host port, conn)
        _                      -> return ()
      loop sock

-- | Pretty print peer address in human readable form.
ppPeer :: PeerAddr -> Doc
ppPeer p @ PeerAddr {..} = case peerID of
    Just pid -> ppClientInfo (clientInfo pid) <+> "at" <+> paddr
    Nothing  -> paddr
  where
    paddr = text (show (peerSockAddr p))
