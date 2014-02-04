-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Protocol independent bittorrent tracker API.
--
module Network.BitTorrent.Tracker.RPC
       ( PeerInfo (..)

         -- * Manager
       , Options (..)
       , Manager
       , newManager
       , closeManager
       , withManager

         -- * RPC
       , SAnnounceQuery (..)
       , announce
       , scrape
       ) where

import Control.Exception
import Data.Default
import Network
import Network.URI
import Network.Socket (HostAddress)

import           Data.Torrent.InfoHash
import           Data.Torrent.Progress
import           Network.BitTorrent.Core
import           Network.BitTorrent.Tracker.Message
import qualified Network.BitTorrent.Tracker.RPC.HTTP as HTTP
import qualified Network.BitTorrent.Tracker.RPC.UDP  as UDP


{-----------------------------------------------------------------------
--  Simplified announce
-----------------------------------------------------------------------}

-- | Info to advertise to trackers.
data PeerInfo = PeerInfo
  { peerId   :: !PeerId
  , peerPort :: !PortNumber
  , peerIP   :: !(Maybe HostAddress)
  } deriving (Show, Eq)

-- | Simplified announce query.
data SAnnounceQuery = SAnnounceQuery
  { sInfoHash :: InfoHash
  , sProgress :: Progress
  , sNumWant  :: Maybe Int
  , sEvent    :: Maybe Event
  }

fillAnnounceQuery :: PeerInfo  ->  SAnnounceQuery  ->    AnnounceQuery
fillAnnounceQuery    PeerInfo{..} SAnnounceQuery {..} = AnnounceQuery
  { reqInfoHash = sInfoHash
  , reqPeerId   = peerId
  , reqPort     = peerPort
  , reqProgress = sProgress
  , reqIP       = peerIP
  , reqNumWant  = sNumWant
  , reqEvent    = sEvent
  }

{-----------------------------------------------------------------------
--  RPC manager
-----------------------------------------------------------------------}

-- | Tracker manager settings.
data Options = Options
  { -- | HTTP tracker protocol specific options.
    optHttpRPC      :: !HTTP.Options

    -- | UDP tracker protocol specific options.
  , optUdpRPC       :: !UDP.Options

    -- | Whether to use multitracker extension.
  , optMultitracker :: !Bool
  }

instance Default Options where
  def = Options
    { optHttpRPC      = def
    , optUdpRPC       = def
    , optMultitracker = True
    }

-- | Tracker RPC Manager.
data Manager  = Manager
  { options  :: !Options
  , peerInfo :: !PeerInfo
  , httpMgr  :: !HTTP.Manager
  , udpMgr   :: !UDP.Manager
  }

-- | Normally a bittorrent client session need a single RPC manager
-- only.
newManager :: Options -> PeerInfo -> IO Manager
newManager opts info = do
  h <- HTTP.newManager (optHttpRPC opts)
  u <-  UDP.newManager (optUdpRPC  opts) `onException` HTTP.closeManager h
  return $ Manager opts info h u

closeManager :: Manager -> IO ()
closeManager Manager {..} = do
  UDP.closeManager udpMgr `finally` HTTP.closeManager httpMgr

withManager :: Options -> PeerInfo -> (Manager -> IO a) -> IO a
withManager opts info = bracket (newManager opts info) closeManager

{-----------------------------------------------------------------------
--  RPC calls
-----------------------------------------------------------------------}
-- TODO Catch IO exceptions on rpc calls.

dispatch :: URI -> IO a -> IO a -> IO a
dispatch URI {..} http udp
  | uriScheme == "http:" = http
  | uriScheme == "udp:"  = udp
  |       otherwise      = throwIO $ userError msg
  where
    msg  = "unknown tracker protocol scheme: " ++ show uriScheme

announce :: Manager -> URI -> SAnnounceQuery -> IO AnnounceInfo
announce Manager {..} uri simpleQuery
  = dispatch uri
      (HTTP.announce httpMgr uri annQ)
      ( UDP.announce udpMgr  uri annQ)
  where
    annQ = fillAnnounceQuery peerInfo simpleQuery

scrape :: Manager -> URI -> ScrapeQuery -> IO ScrapeInfo
scrape Manager {..} uri q
  = dispatch uri
      (HTTP.scrape httpMgr uri q)
      ( UDP.scrape udpMgr  uri q)
