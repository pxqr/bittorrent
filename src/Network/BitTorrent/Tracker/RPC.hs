-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
module Network.BitTorrent.Tracker.RPC
       ( Tracker
       , Network.BitTorrent.Tracker.RPC.connect
       , Network.BitTorrent.Tracker.RPC.announce
       , Network.BitTorrent.Tracker.RPC.scrape
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans.Resource
import Network.URI

import Network.BitTorrent.Tracker.Message
import Network.BitTorrent.Tracker.RPC.HTTP as HTTP
import Network.BitTorrent.Tracker.RPC.UDP  as UDP


data Tracker
  = HTracker Connection
  | UTracker UDPTracker

connect :: URI -> IO Tracker
connect uri @ URI {..}
  | uriScheme == "http:" = HTracker <$> runResourceT (HTTP.connect uri)
  | uriScheme == "udp:"  = UTracker <$> UDP.connect uri
  |       otherwise      = throwIO $ userError msg
  where
    msg = "unknown tracker protocol scheme: " ++ show uriScheme

announce :: AnnounceQuery -> Tracker -> IO AnnounceInfo
announce q (HTracker t) = runResourceT $ HTTP.announce q t
announce q (UTracker t) = UDP.announce q t

scrape :: ScrapeQuery -> Tracker -> IO ScrapeInfo
scrape q (HTracker t) = runResourceT $ HTTP.scrape q t
scrape q (UTracker t) = UDP.scrape q t
