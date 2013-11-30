module Network.BitTorrent.Tracker.RPC
       ( module Network.BitTorrent.Tracker.RPC.Message
       , TrackerRPC (..)
       ) where

import Network.BitTorrent.Tracker.RPC.Message
import Network.BitTorrent.Tracker.RPC.HTTP as HTTP
import Network.BitTorrent.Tracker.RPC.UDP  as UDP

-- | Set of tracker RPCs.
class Tracker s where
  connect  :: URI -> IO s
  announce :: s -> AnnounceQuery -> IO AnnounceInfo
  scrape   :: s -> ScrapeQuery   -> IO Scrape

instance Tracker HTTP.Tracker where
  connect  = return . HTTP.Tracker
  announce = HTTP.announce
  scrape   = undefined

instance Tracker UDP.Tracker where
  connect  = initialTracker
  announce = announce
  scrape   = undefined

data BitTracker = HTTPTr HTTPTracker
                | UDPTr UDPTracker

instance Tracker BitTracker where
  connect uri @ URI {..}
    | uriScheme == "udp:"  = UDPTr  <$> connect uri
    | uriScheme == "http:" = HTTPTr <$> connect uri
    |       otherwise      = throwIO $ userError msg
    where
      msg = "unknown tracker protocol scheme: " ++ show uriScheme

  announce (HTTPTr t) = Tracker.announce t
  announce (UDPTr  t) = Tracker.announce t

  scrape (HTTPTr t) = scrape t
  scrape (UDPTr  t) = scrape t
