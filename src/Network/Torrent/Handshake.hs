{-# LANGUAGE OverloadedStrings #-}
module Network.Torrent.Handshake
       ( Handshake
       , defaultProtocol, defaultReserved, defaultHandshake
       ) where

import Control.Applicative
import Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize

import Network.Torrent.PeerID

-- | In order to establish the connection between peers we should send 'Handshake'
--   message. The 'Handshake' is a required message and must be the first message
--   transmitted by the peer to the another peer.
data Handshake = Handshake {
    hsProtocol    :: ByteString  -- ^ Identifier of the protocol.
  , hsReserved    :: Word64      -- ^ Reserved bytes, rarely used.
  , hsInfoHash    :: ByteString  -- ^ Hash from the metainfo file.
    -- This /should be/ same hash that is transmitted in tracker requests.
  , hsPeerID      :: PeerID      -- ^ Peer id of the initiator.
    -- This is /usually the same peer id that is transmitted in tracker requests.
  } deriving (Show, Eq)

instance Serialize Handshake where
  put hs = do
    putWord8 (fromIntegral (B.length (hsProtocol hs)))
    putByteString (hsProtocol hs)
    putWord64be   (hsReserved hs)
    putByteString (hsInfoHash hs)
    put (hsPeerID hs)

  get = do
    len  <- getWord8
    Handshake <$> getBytes (fromIntegral len)
              <*> getWord64be
              <*> getBytes 20
              <*> get

-- | Default protocol string "BitTorrent protocol" as is.
defaultProtocol :: ByteString
defaultProtocol = "BitTorrent protocol"

-- | Default reserved word is 0.
defaultReserved :: Word64
defaultReserved = 0

-- | Length of info hash and peer id is unchecked, so it /should/ be equal 20.
defaultHandshake :: ByteString -- ^ Info hash string.
                 -> PeerID
                 -> Handshake
defaultHandshake hash pid = Handshake defaultProtocol defaultReserved hash pid