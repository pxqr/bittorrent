-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   In order to establish the connection between peers we should send
--   'Handshake' message. The 'Handshake' is a required message and
--   must be the first message transmitted by the peer to the another
--   peer.
--
{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.PeerWire.Handshake
       ( Handshake
       , handshake
       , ppHandshake
       , defaultHandshake, defaultBTProtocol, defaultReserved
       , handshakeMaxSize
       ) where

import Control.Applicative
import Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Serialize as S
import Data.Torrent.InfoHash
import Network
import Network.Socket.ByteString

import Network.BitTorrent.Peer.ID
import Network.BitTorrent.Peer.ClientInfo


data Handshake = Handshake {
    -- | Identifier of the protocol.
    hsProtocol    :: ByteString

    -- | Reserved bytes used to specify supported BEP's.
  , hsReserved    :: Word64

    -- | Info hash of the info part of the metainfo file. that is
    -- transmitted in tracker requests. Info hash of the initiator
    -- handshake and response handshake should match, otherwise
    -- initiator should break the connection.
    --
  , hsInfoHash    :: InfoHash

    -- | Peer id of the initiator. This is usually the same peer id
    -- that is transmitted in tracker requests.
    --
  , hsPeerID      :: PeerID

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

-- TODO add reserved bits info
-- | Format handshake in human readable form.
ppHandshake :: Handshake -> String
ppHandshake hs = BC.unpack (hsProtocol hs) ++ " "
              ++ ppClientInfo (clientInfo (hsPeerID hs))

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


-- | Handshaking with a peer specified by the second argument.
--
handshake :: Socket -> Handshake -> IO (Either String Handshake)
handshake sock hs = do
    sendAll sock (S.encode hs)
    r <- recv sock handshakeMaxSize
    return (checkIH (S.decode r))
  where
    checkIH (Right hs')
      | hsInfoHash hs /= hsInfoHash hs' = Left "Handshake info hash do not match."
    checkIH x = x
