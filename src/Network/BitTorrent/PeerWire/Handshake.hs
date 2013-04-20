-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.PeerWire.Handshake
       ( Handshake
       , handshakeMaxSize
       , defaultBTProtocol, defaultReserved, defaultHandshake
       , handshake
       ) where

import Control.Applicative
import Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize as S
import Data.Torrent.InfoHash
import Network
import Network.Socket.ByteString

import Network.BitTorrent.PeerID


-- | In order to establish the connection between peers we should send 'Handshake'
--   message. The 'Handshake' is a required message and must be the first message
--   transmitted by the peer to the another peer.
data Handshake = Handshake {
    hsProtocol    :: ByteString  -- ^ Identifier of the protocol.
  , hsReserved    :: Word64      -- ^ Reserved bytes, rarely used.
  , hsInfoHash    :: InfoHash    -- ^ Hash from the metainfo file.
    -- This /should be/ same hash that is transmitted in tracker requests.
  , hsPeerID      :: PeerID      -- ^ Peer id of the initiator.
    -- This is /usually the same peer id that is transmitted in tracker requests.
  } deriving (Show, Eq)

instance Serialize Handshake where
  put hs = do
    putWord8 (fromIntegral (B.length (hsProtocol hs)))
    putByteString (hsProtocol hs)
    putWord64be   (hsReserved hs)
    put (hsInfoHash hs)
    put (hsPeerID hs)

  get = do
    len  <- getWord8
    Handshake <$> getBytes (fromIntegral len)
              <*> getWord64be
              <*> get
              <*> get

-- | Maximum size of handshake message in bytes.
handshakeMaxSize :: Int
handshakeMaxSize = 1 + 256 + 8 + 20 + 20

-- | Default protocol string "BitTorrent protocol" as is.
defaultBTProtocol :: ByteString
defaultBTProtocol = "BitTorrent protocol"

-- | Default reserved word is 0.
defaultReserved :: Word64
defaultReserved = 0

-- | Length of info hash and peer id is unchecked, so it /should/ be equal 20.
defaultHandshake :: InfoHash -> PeerID -> Handshake
defaultHandshake = Handshake defaultBTProtocol defaultReserved


-- TODO check if hash the same
-- | Handshaking with a peer specified by the second argument.
--
handshake :: Socket -> Handshake -> IO (Either String Handshake)
handshake sock hs = do
  sendAll sock (S.encode hs)
  r <- recv sock handshakeMaxSize
  return (S.decode r)
