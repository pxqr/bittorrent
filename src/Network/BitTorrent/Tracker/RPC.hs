-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides unified RPC interface to BitTorrent
--   trackers. The tracker is an UDP/HTTP/HTTPS service used to
--   discovery peers for a particular existing torrent and keep
--   statistics about the swarm. This module also provides a way to
--   request scrape info for a particular torrent list.
--
{-# LANGUAGE DeriveDataTypeable #-}
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
       , RpcException (..)
       , Network.BitTorrent.Tracker.RPC.announce
       , scrape
       ) where

import Control.Exception
import Data.Default
import Data.Typeable
import Network
import Network.URI
import Network.Socket (HostAddress)

import           Data.Torrent
import           Network.BitTorrent.Address
import           Network.BitTorrent.Internal.Progress
import           Network.BitTorrent.Tracker.Message
import qualified Network.BitTorrent.Tracker.RPC.HTTP as HTTP
import qualified Network.BitTorrent.Tracker.RPC.UDP  as UDP


{-----------------------------------------------------------------------
--  Simplified announce
-----------------------------------------------------------------------}

-- | Info to advertise to trackers.
data PeerInfo = PeerInfo
  { peerId   :: !PeerId
  , peerIP   :: !(Maybe HostAddress)
  , peerPort :: !PortNumber
  } deriving (Show, Eq)

instance Default PeerInfo where
  def = PeerInfo def Nothing 6881

-- | Simplified announce query.
data SAnnounceQuery = SAnnounceQuery
  { sInfoHash :: InfoHash
  , sProgress :: Progress
  , sNumWant  :: Maybe Int
  , sEvent    :: Maybe AnnounceEvent
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

-- | Create a new 'Manager'. You /must/ manually 'closeManager'
-- otherwise resource leakage is possible. Normally, a bittorrent
-- client need a single RPC manager only.
--
--   This function can throw 'IOException' on invalid 'Options'.
--
newManager :: Options -> PeerInfo -> IO Manager
newManager opts info = do
  h <- HTTP.newManager (optHttpRPC opts)
  u <-  UDP.newManager (optUdpRPC  opts) `onException` HTTP.closeManager h
  return $ Manager opts info h u

-- | Close all pending RPCs. Behaviour of currently in-flight RPCs can
-- differ depending on underlying protocol used. No rpc calls should
-- be performed after manager becomes closed.
closeManager :: Manager -> IO ()
closeManager Manager {..} = do
  UDP.closeManager udpMgr `finally` HTTP.closeManager httpMgr

-- | Normally you need to use 'Control.Monad.Trans.Resource.allocate'.
withManager :: Options -> PeerInfo -> (Manager -> IO a) -> IO a
withManager opts info = bracket (newManager opts info) closeManager

{-----------------------------------------------------------------------
--  Exceptions
-----------------------------------------------------------------------}
-- TODO Catch IO exceptions on rpc calls (?)

data RpcException
  = UdpException    UDP.RpcException  -- ^ UDP RPC driver failure;
  | HttpException   HTTP.RpcException -- ^ HTTP RPC driver failure;
  | UnrecognizedScheme   String       -- ^ unsupported scheme in announce URI;
  | GenericException     String       -- ^ for furter extensibility.
    deriving (Show, Typeable)

instance Exception RpcException

packException :: Exception e => (e -> RpcException) -> IO a -> IO a
packException f m = try m >>= either (throwIO . f) return
{-# INLINE packException #-}

{-----------------------------------------------------------------------
--  RPC calls
-----------------------------------------------------------------------}

dispatch :: URI -> IO a -> IO a -> IO a
dispatch URI {..} http udp
  | uriScheme == "http:" ||
    uriScheme == "https:" = packException HttpException http
  | uriScheme == "udp:"   = packException UdpException  udp
  |       otherwise       = throwIO $ UnrecognizedScheme uriScheme

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
