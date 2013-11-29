-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides peer capabilities detection.
--
--   See <http://www.bittorrent.org/beps/bep_0004.html> for more
--   information.
--
module Network.BitTorrent.Exchange.Extension
       ( -- * Capabilities
         Capabilities
       , ppCaps, defaultCaps
       , enabledCaps

         -- * Extensions
       , Extension(..)
       , defaultExtensions, ppExtension
       , encodeExts, decodeExts
       ) where

import Data.Bits
import Data.Word
import Text.PrettyPrint


type Capabilities = Word64

ppCaps :: Capabilities -> Doc
ppCaps = hcat . punctuate ", " . map ppExtension . decodeExts

defaultCaps :: Capabilities
defaultCaps = 0

enabledCaps :: Capabilities -- ^ of the client.
            -> Capabilities -- ^ of the peer.
            -> Capabilities -- ^ should be considered as enabled.
enabledCaps = (.&.)


data Extension = ExtDHT  -- ^ BEP 5
               | ExtFast -- ^ BEP 6
                 deriving (Show, Eq, Ord, Enum, Bounded)

ppExtension :: Extension -> Doc
ppExtension ExtDHT  = "DHT"
ppExtension ExtFast = "Fast Extension"

extensionMask :: Extension -> Word64
extensionMask ExtDHT  = 0x01
extensionMask ExtFast = 0x04

defaultExtensions :: [Extension]
defaultExtensions = []

encodeExts :: [Extension] -> Capabilities
encodeExts = foldr (.&.) 0 . map extensionMask

decodeExts :: Capabilities -> [Extension]
decodeExts rb = filter (testMask rb . extensionMask) [minBound..maxBound]
  where
    testMask bits x = bits .&. x > 0