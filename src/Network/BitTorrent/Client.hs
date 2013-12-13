module Network.BitTorrent.Client
       ( Options (..)
       , Client
       , newClient
       , addTorrent
       ) where

import Control.Concurrent.STM
import Data.Default
import Data.Function
import Data.HashMap.Strict as HM
import Data.Ord
import Data.Text
import Network

import Data.Torrent
import Data.Torrent.InfoHash
import Network.BitTorrent.Client.Swarm
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Message


data Options = Options
  { fingerprint :: Fingerprint
  , name        :: Text
  , port        :: PortNumber
  , extensions  :: [Extension]
  }

instance Default Options where
  def = Options
    { fingerprint = def
    , name        = "hs-bittorrent"
    , port        = 6882
    , extensions  = []
    }

data Client = Client
  { clientPeerId       :: !PeerId
  , clientListenerPort :: !PortNumber
  , allowedExtensions  :: !Caps
  , torrents           :: TVar (HashMap InfoHash Swarm)
  }

instance Eq Client where
  (==) = (==) `on` clientPeerId

instance Ord Client where
  compare = comparing clientPeerId

newClient :: Options -> IO Client
newClient Options {..} = do
  pid <- genPeerId
  ts  <- newTVarIO HM.empty
  return Client
    { clientPeerId       = pid
    , clientListenerPort = port
    , allowedExtensions  = toCaps extensions
    , torrents           = ts
    }

addTorrent :: Torrent -> Client -> IO ()
addTorrent t Client {..} = do
  leecher <- newLeecher clientPeerId clientListenerPort t
  let ih = idInfoHash (tInfoDict t)
  atomically $ modifyTVar' torrents (HM.insert ih leecher)
  askPeers leecher >>= print