module Network.BitTorrent.Client.Swarm
       ( Swarm
       , newLeecher
       , askPeers
       ) where

import Data.Default
import Data.Maybe
import Network

import Data.Torrent
import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.Tracker.Message
import Network.BitTorrent.Tracker.RPC as RPC


data Swarm = Swarm
  { swarmTopic   :: InfoHash
  , thisPeerId   :: PeerId
  , listenerPort :: PortNumber
  }

newLeecher :: PeerId -> PortNumber -> Torrent -> IO Swarm
newLeecher pid port Torrent {..} = do
  return Swarm
    { swarmTopic   = idInfoHash tInfoDict
    , thisPeerId   = pid
    , listenerPort = port
    }

getAnnounceQuery :: Swarm -> AnnounceQuery
getAnnounceQuery Swarm {..} = AnnounceQuery
  { reqInfoHash = swarmTopic
  , reqPeerId   = thisPeerId
  , reqPort     = listenerPort
  , reqProgress = def
  , reqIP       = Nothing
  , reqNumWant  = Nothing
  , reqEvent    = Nothing
  }

askPeers :: Swarm -> IO [PeerAddr IP]
askPeers s @ Swarm {..} = do
--  AnnounceInfo {..} <- RPC.announce (getAnnounceQuery s) trackerConn
  return [] -- (getPeerList respPeers)

--reannounce :: HTracker -> IO ()
--reannounce = undefined

--forceReannounce :: HTracker -> IO ()
--forceReannounce = undefined
