{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- TODO: tests
-- | Recommended method for generation of the peer ID's is 'newPeerID',
--   though this module exports some other goodies for custom generation.
--
module Network.Torrent.PeerID
       ( PeerID (getPeerID)
         -- * Encoding styles
       , azureusStyle, shadowStyle
         -- * Defaults
       , defaultClientID, defaultVersionNumber
         -- * Generation
       , newPeerID, timestampByteString
         -- * Extra
       , byteStringPadded
       ) where

import Control.Applicative
import Data.BEncode
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import Data.Foldable    (foldMap)
import Data.Monoid      ((<>))
import Data.Serialize   (Serialize)
import Data.Version     (Version(Version), versionBranch)
import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)

-- TODO we have linker error here, so manual hardcoded version for a while.
-- import Paths_network_bittorrent (version)
version :: Version
version = Version [0, 10, 0, 0] []

-- | Peer identifier is exactly 20 bytes long bytestring.
newtype PeerID = PeerID { getPeerID :: ByteString }
                 deriving (Show, Eq, Ord, BEncodable, Serialize)

-- | Azureus-style encoding:
--     * 1  byte : '-'
--     * 2  bytes: client id
--     * 4  bytes: version number
--     * 1  byte : '-'
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

-- | Shadow-style encoding:
--     * 1 byte   : client id.
--     * 0-4 bytes: version number. If less than 4 then padded with '-' char.
--     * 15 bytes : random number. If length is less than 15 then padded with '0' char.
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

-- | Gives exactly 4 bytes long version number for any version of the package.
--   Version is taken from .cabal.
defaultVersionNumber :: ByteString
defaultVersionNumber = B.take 4 (BC.pack (foldMap show (versionBranch version)))

-- | Gives 15 characters long decimal timestamp such that:
--     * 6 bytes   : first 6 characters from picoseconds obtained with %q.
--     * 1 bytes   : character '.' for readability.
--     * 9..* bytes: number of whole seconds since the Unix epoch (!)REVERSED.
--   Can be used both with shadow and azureus style encoding. This format is
--   used to make the ID's readable(for debugging) and more or less random.
--
timestampByteString :: IO ByteString
timestampByteString = (BC.pack . format) <$> getCurrentTime
  where
    format t = take 6 (formatTime defaultTimeLocale "%q" t) ++ "." ++
               take 9 (reverse (formatTime defaultTimeLocale "%s" t))

-- |  Here we use Azureus-style encoding with the following args:
--      * 'HS' for the client id.
--      * Version of the package for the version number
--      * UTC time day ++ day time for the random number.
--
newPeerID :: IO PeerID
newPeerID = azureusStyle defaultClientID defaultVersionNumber
                                     <$> timestampByteString

-- | length < size: Complete bytestring by given charaters.
--   length = size: Output bytestring as is.
--   length > size: Drop last (length - size) charaters from a given bytestring.
--
byteStringPadded :: ByteString -- ^ bytestring to be padded.
                 -> Int        -- ^ size of result builder.
                 -> Char       -- ^ character used for padding.
                 -> B.Builder
byteStringPadded bs s c =
      B.byteString (B.take s bs) <>
      B.byteString (BC.replicate padLen c)
  where
    padLen = s - max (B.length bs) s