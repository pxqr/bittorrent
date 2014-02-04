-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  provisional
--   Portability :  portable
--
--   The tracker is an HTTP/HTTPS service used to discovery peers for
--   a particular existing torrent and keep statistics about the
--   swarm. This module also provides a way to easily request scrape
--   info for a particular torrent list.
--
--   For more information see:
--   <https://wiki.theory.org/BitTorrentSpecification#Tracker_HTTP.2FHTTPS_Protocol>
--
module Network.BitTorrent.Tracker.RPC.HTTP
       ( -- * Manager
         Options (..)
       , Manager
       , newManager
       , closeManager
       , withManager

         -- * RPC
       , announce
       , scrape
       , scrapeOne
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans.Resource
import Data.BEncode as BE
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy  as BL
import Data.Default
import Data.List as L
import Data.Monoid
import Network.URI
import           Network.HTTP.Conduit hiding
                 (Manager, newManager, closeManager, withManager)
import qualified Network.HTTP.Conduit as HTTP
import           Network.HTTP.Conduit.Internal (setUri)
import           Network.HTTP.Types.Header (hUserAgent)
import           Network.HTTP.Types.URI    (SimpleQuery, renderSimpleQuery)

import Data.Torrent.InfoHash               (InfoHash)
import Network.BitTorrent.Core.Fingerprint (libUserAgent)
import Network.BitTorrent.Tracker.Message

{-----------------------------------------------------------------------
--  Manager
-----------------------------------------------------------------------}

-- | HTTP tracker specific RPC options.
data Options = Options
  { -- | Global HTTP announce query preferences.
    optAnnounceExt  :: !AnnounceQueryExt

    -- | Whether to use HTTP proxy for HTTP tracker requests.
  , optHttpProxy    :: !(Maybe Proxy)

    -- | Value to put in HTTP user agent header.
  , optUserAgent    :: !BS.ByteString

    -- | HTTP manager options.
  , optHttpOptions  :: !ManagerSettings
  }

instance Default Options where
  def = Options
    { optAnnounceExt = def
    , optHttpProxy   = Nothing
    , optUserAgent   = BC.pack libUserAgent
    , optHttpOptions = def
    }

-- | HTTP tracker manager.
data Manager = Manager
  { options :: !Options
  , httpMgr :: !HTTP.Manager
  }

newManager :: Options -> IO Manager
newManager opts = Manager opts <$> HTTP.newManager (optHttpOptions opts)

closeManager :: Manager -> IO ()
closeManager Manager {..} = HTTP.closeManager httpMgr

withManager :: Options -> (Manager -> IO a) -> IO a
withManager opts = bracket (newManager opts) closeManager

{-----------------------------------------------------------------------
--  Queries
-----------------------------------------------------------------------}

fillRequest :: Options -> SimpleQuery -> Request m -> Request m
fillRequest Options {..} q r = r
  { queryString    = joinQuery (queryString r) (renderSimpleQuery False q)
  , requestHeaders = (hUserAgent, optUserAgent) : requestHeaders r
  , proxy          = optHttpProxy
  }
  where
    joinQuery a b
      | BS.null a = b
      | otherwise = a <> "&" <> b

httpTracker :: BEncode a => Manager -> URI -> SimpleQuery -> ResourceT IO a
httpTracker Manager {..} uri q = do
  request  <- fillRequest options q <$> setUri def uri
  response <- httpLbs request httpMgr
  case BE.decode $ BL.toStrict $ responseBody response of
    Left  msg  -> error $ "httpTracker: " ++ msg
    Right info -> return info

{-----------------------------------------------------------------------
--  RPC
-----------------------------------------------------------------------}

-- | Send request and receive response from the tracker specified in
-- announce list.
--
announce :: Manager -> URI -> AnnounceQuery -> ResourceT IO AnnounceInfo
announce mgr uri q = httpTracker mgr uri (renderAnnounceRequest uriQ)
  where
    uriQ = AnnounceRequest
      { announceQuery   = q
      , announceAdvises = optAnnounceExt (options mgr)
      }

-- | Trying to convert /announce/ URL to /scrape/ URL. If 'scrapeURL'
--   gives 'Nothing' then tracker do not support scraping.
--
scrapeURL :: URI -> Maybe URI
scrapeURL uri = do
    newPath <- replace (BC.pack (uriPath uri))
    return uri { uriPath = BC.unpack newPath }
  where
    replace p
      | ps <- BC.splitWith (== '/') p
      , "announce" `BS.isPrefixOf` L.last ps
      = let newSuff = "scrape" <> BS.drop (BS.length "announce") (L.last ps)
        in Just (BS.intercalate "/" (L.init ps ++ [newSuff]))
      | otherwise = Nothing

-- | For each 'InfoHash' of torrents request scrape info from the tracker.
--   However if the info hash list is 'null', the tracker should list
--   all available torrents.
--
scrape :: Manager -> URI -> ScrapeQuery -> ResourceT IO ScrapeInfo
scrape m u q = do
  case scrapeURL u of
    Nothing  -> error "Tracker do not support scraping"
    Just uri -> httpTracker m uri (renderScrapeQuery q)

-- | More particular version of 'scrape', just for one torrent.
--
scrapeOne :: Manager -> URI -> InfoHash -> ResourceT IO ScrapeEntry
scrapeOne m uri ih = do
  xs <- scrape m uri [ih]
  case L.lookup ih xs of
    Nothing -> error "unable to find info hash in response dict"
    Just a  -> return a
