{-# LANGUAGE OverloadedStrings #-}
module Network.Torrent.Handshake
       ( Handshake
       ) where

import Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

data Handshake = Handshake {
    hsProtocol    :: ByteString
  , hsReserved    :: Word64
  , hsInfoHash    :: ByteString
  , hsPeerID      :: ByteString
  } deriving (Show, Eq)

defaultProtocol :: ByteString
defaultProtocol = "BitTorrent protocol"


fromByteString :: ByteString -> Handshake
fromByteString = undefined

toByteString :: Handshake -> ByteString
toByteString = undefined