-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
-- The return value for a query for peers includes an opaque value
-- known as the 'Token'. For a node to announce that its controlling
-- peer is downloading a torrent, it must present the token received
-- from the same queried node in a recent query for peers. When a node
-- attempts to \"announce\" a torrent, the queried node checks the
-- token against the querying node's 'IP' address. This is to prevent
-- malicious hosts from signing up other hosts for torrents. Since the
-- token is merely returned by the querying node to the same node it
-- received the token from, the implementation is not defined. Tokens
-- must be accepted for a reasonable amount of time after they have
-- been distributed.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BitTorrent.DHT.Token
       ( -- * Token
         Token

         -- * Session tokens
       , TokenMap

         -- ** Construction
       , Network.BitTorrent.DHT.Token.tokens

         -- ** Query
       , Network.BitTorrent.DHT.Token.lookup
       , Network.BitTorrent.DHT.Token.member

         -- ** Modification
       , Network.BitTorrent.DHT.Token.defaultUpdateInterval
       , Network.BitTorrent.DHT.Token.update
       ) where

import Control.Applicative
import Control.Monad.State
import Data.BEncode (BEncode)
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Builder as BS
import Data.Default
import Data.List as L
import Data.Hashable
import Data.String
import Data.Time
import System.Random

import Network.BitTorrent.Address

-- TODO use ShortByteString

-- | An opaque value.
newtype Token = Token BS.ByteString
  deriving (Show, Eq, BEncode, IsString)

-- | Meaningless token, for testing purposes only.
instance Default Token where
  def = Token "0xdeadbeef"

-- | The secret value used as salt.
type Secret = Int

-- The BitTorrent implementation uses the SHA1 hash of the IP address
-- concatenated onto a secret, we use hashable instead.
makeToken :: Hashable a => NodeAddr a -> Secret -> Token
makeToken n s = Token $ toBS $ hashWithSalt s n
  where
    toBS = toStrict . toLazyByteString . int64BE . fromIntegral

-- | Constant space 'Node' to 'Token' map based on the secret value.
data TokenMap = TokenMap
  { prevSecret :: {-# UNPACK #-} !Secret
  , curSecret  :: {-# UNPACK #-} !Secret
  , generator  :: {-# UNPACK #-} !StdGen
  } deriving Show

-- | A new token map based on the specified seed value. Returned token
-- map should be periodicatically 'update'd.
--
-- Normally, the seed value should vary between invocations of the
-- client software.
tokens :: Int -> TokenMap
tokens seed = (`evalState` mkStdGen seed) $
  TokenMap <$> state next
           <*> state next
           <*> get

-- | Get token for the given node. A token becomes invalid after 2
-- 'update's.
--
-- Typically used to handle find_peers query.
lookup :: Hashable a => NodeAddr a -> TokenMap -> Token
lookup addr TokenMap {..} = makeToken addr curSecret

-- | Check if token is valid.
--
-- Typically used to handle 'Network.BitTorrent.DHT.Message.Announce'
-- query. If token is invalid the 'Network.KRPC.ProtocolError' should
-- be sent back to the malicious node.
member :: Hashable a => NodeAddr a -> Token -> TokenMap -> Bool
member addr token TokenMap {..} = token `L.elem` valid
  where valid = makeToken addr <$> [curSecret, prevSecret]

-- | Secret changes every five minutes and tokens up to ten minutes old
-- are accepted.
defaultUpdateInterval :: NominalDiffTime
defaultUpdateInterval = 5 * 60

-- | Update current tokens.
update :: TokenMap -> TokenMap
update TokenMap {..} = TokenMap
    { prevSecret = curSecret
    , curSecret  = newSecret
    , generator  = newGen
    }
  where
    (newSecret, newGen) = next generator