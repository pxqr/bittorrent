module Network.BitTorrent.DHT.ContactInfo () where
import Data.HashMap.Strict as HM

import Data.Torrent.InfoHash
import Network.BitTorrent.Core

-- increase prefix when table is too large
-- decrease prefix when table is too small
-- filter outdated peers

data ContactInfo ip = PeerStore
  { maxSize    :: Int
  , prefixSize :: Int
  , thisNodeId :: NodeId

  , count      :: Int  -- ^ Cached size of the 'peerSet'
  , peerSet    :: HashMap InfoHash [PeerAddr ip]
  }
