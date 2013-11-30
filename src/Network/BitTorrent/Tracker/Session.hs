module Network.BitTorrent.Tracker.Session
       (
       ) where

import Data.Torrent.Progress
import Data.Torrent.InfoHash
import Network.BitTorrent.Core.PeerAddr
import Network.BitTorrent.Tracker.Message

data PeerInfo = PeerInfo
  { peerId   :: PeerId
  , peerPort :: PortNumber
  , peerIP   :: Maybe HostAddress
  }

data Session  = Session
  { sesInfoHash :: !InfoHash
  , sesPeerInfo :: !PeerInfo
  }

data SAnnounceQuery = SAnnounceQuery
  { sreqProgress :: Progress
  , sreqNumWant  :: Maybe Int
  , sreqEvent    :: Maybe Event
  }

type SAnnounceInfo = [PeerAddr]

f :: Session  ->  SAnnounceQuery  ->    AnnounceQuery
f    Session {..} SAnnounceQuery {..} = AnnounceQuery
  { reqInfoHash = sesInfoHash
  , reqPeerInfo = sesPeerInfo
  , reqProgress = sreqProgress
  , reqNumWant  = undefined
  , reqEvent    = sreqEvent
  }

data Settings = Settings

data Manager  = Manager
  {
  }


g :: Session -> AnnounceInfo -> SAnnounceInfo
g Session {..} SAnnounceInfo {..} = undefined


reannounce :: HTracker -> IO ()
reannounce = undefined

forceReannounce :: HTracker -> IO ()
forceReannounce = undefined

scrape :: HTracker -> IO ()
scrape = undefined
