module Network.BitTorrent.Client
       ( Options (..)
       , Client (..)
       ) where

import Data.Default
import Data.Function
import Data.Ord
import Data.Text
import Network

import Data.Torrent
import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Message


data Options = Options
  { fingerprint :: Fingerprint
  , name        :: Text
  , port        :: PortNumber
  }

instance Default Options where
  def = Options
    { fingerprint = def
    , name        = "hs-bittorrent"
    , port        = 6882
    }

data Client = Client
  { clientPeerId      :: !PeerId
  , allowedExtensions :: !Caps
  }

instance Eq Client where
  (==) = (==) `on` clientPeerId

instance Ord Client where
  compare = comparing clientPeerId
