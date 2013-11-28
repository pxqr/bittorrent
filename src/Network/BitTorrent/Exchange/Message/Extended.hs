-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   For more info see <http://www.bittorrent.org/beps/bep_0010.html>
--
{-# LANGUAGE DeriveDataTypeable #-}
module Network.BitTorrent.Exchange.Message.Extended
       (
       ) where

import Data.BEncode
import Data.IntMap as IM
import Data.Text
import Data.Typeable
import Network
import Network.Socket

import Network.BitTorrent.Core.PeerAddr


type Extension = ()

type ExtMap = IntMap Extension

data ExtendedHandshake = H
  { extMap  :: ExtMap
  , port    :: Maybe PortNumber
  , version :: Maybe Text -- TODO ClientInfo
  , yourip  :: Maybe SockAddr
--  , ipv6 , ipv4

    -- | The number of outstanding 'Request' messages this
    -- client supports without dropping any.
  , requestQueueLength :: Maybe Int
  } deriving (Show, Typeable)

instance BEncode ExtendedHandshake where
  toBEncode H {..} = toDict $
       "p" .=? port
    .: endDict

  fromBEncode = fromDict $ do
    undefined

data ExtendedMessage
  = ExtendedHandshake
    deriving (Show, Eq)
