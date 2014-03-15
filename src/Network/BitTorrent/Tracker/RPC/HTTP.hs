-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  provisional
--   Portability :  portable
--
--   This module implement HTTP tracker protocol.
--
--   For more information see:
--   <https://wiki.theory.org/BitTorrentSpecification#Tracker_HTTP.2FHTTPS_Protocol>
--
{-# LANGUAGE DeriveDataTypeable #-}
module Network.BitTorrent.Tracker.RPC.HTTP
       ( -- * Manager
         Options (..)
       , Manager
       , newManager
       , closeManager
       , withManager

         -- * RPC
       , RpcException (..)
       , announce
       , scrape
       , scrapeOne
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Resource
import Data.BEncode as BE
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy  as BL
import Data.Default
import Data.List as L
import Data.Monoid
import Data.Typeable
import Network.URI
import           Network.HTTP.Conduit hiding
                 (Manager, newManager, closeManager, withManager)
import           Network.HTTP.Client (defaultManagerSettings)
import           Network.HTTP.Client.Internal (setUri)
import qualified Network.HTTP.Conduit as HTTP
import           Network.HTTP.Types.Header (hUserAgent)
import           Network.HTTP.Types.URI    (SimpleQuery, renderSimpleQuery)

import Data.Torrent.InfoHash               (InfoHash)
import Network.BitTorrent.Core.Fingerprint (libUserAgent)
import Network.BitTorrent.Tracker.Message

{-----------------------------------------------------------------------
-- Exceptions
-----------------------------------------------------------------------}

data RpcException
  = RequestFailed HttpException -- ^ failed HTTP request.
  | ParserFailure String        -- ^ unable to decode tracker response;
  | ScrapelessTracker           -- ^ tracker do not support scraping;
  | BadScrape                   -- ^ unable to find info hash in response dict;
    deriving (Show, Typeable)

instance Exception RpcException

packHttpException :: IO a -> IO a
packHttpException m = try m >>= either (throwIO . RequestFailed) return

{-----------------------------------------------------------------------
--  Manager
-----------------------------------------------------------------------}

-- | HTTP tracker specific RPC options.
data Options = Options
  { -- | Global HTTP announce query preferences.
    optAnnouncePrefs :: !AnnouncePrefs

    -- | Whether to use HTTP proxy for HTTP tracker requests.
  , optHttpProxy     :: !(Maybe Proxy)

    -- | Value to put in HTTP user agent header.
  , optUserAgent     :: !BS.ByteString

    -- | HTTP manager options.
  , optHttpOptions   :: !ManagerSettings
  }

instance Default Options where
  def = Options
    { optAnnouncePrefs = def
    , optHttpProxy     = Nothing
    , optUserAgent     = BC.pack libUserAgent
    , optHttpOptions   = defaultManagerSettings
    }

-- | HTTP tracker manager.
data Manager = Manager
  { options :: !Options
  , httpMgr :: !HTTP.Manager
  }

-- |
newManager :: Options -> IO Manager
newManager opts = Manager opts <$> HTTP.newManager (optHttpOptions opts)

-- |
closeManager :: Manager -> IO ()
closeManager Manager {..} = HTTP.closeManager httpMgr

-- | Normally you need to use 'Control.Monad.Trans.Resource.allocate'.
withManager :: Options -> (Manager -> IO a) -> IO a
withManager opts = bracket (newManager opts) closeManager

{-----------------------------------------------------------------------
--  Queries
-----------------------------------------------------------------------}

fillRequest :: Options -> SimpleQuery -> Request -> Request
fillRequest Options {..} q r = r
  { queryString    = joinQuery (queryString r) (renderSimpleQuery False q)
  , requestHeaders = (hUserAgent, optUserAgent) : requestHeaders r
  , proxy          = optHttpProxy
  }
  where
    joinQuery a b
      | BS.null a = b
      | otherwise = a <> "&" <> b

httpTracker :: BEncode a => Manager -> URI -> SimpleQuery -> IO a
httpTracker Manager {..} uri q = packHttpException $ do
  request  <- fillRequest options q <$> setUri def uri
  response <- runResourceT $ httpLbs request httpMgr
  case BE.decode $ BL.toStrict $ responseBody response of
    Left  msg  -> throwIO (ParserFailure msg)
    Right info -> return info

{-----------------------------------------------------------------------
--  RPC
-----------------------------------------------------------------------}

-- | Send request and receive response from the tracker specified in
-- announce list.
--
--   This function can throw 'RpcException'.
--
announce :: Manager -> URI -> AnnounceQuery -> IO AnnounceInfo
announce mgr uri q = httpTracker mgr uri (renderAnnounceRequest uriQ)
  where
    uriQ = AnnounceRequest
      { announceQuery = q
      , announcePrefs = optAnnouncePrefs (options mgr)
      }

-- | Trying to convert /announce/ URL to /scrape/ URL. If 'scrapeURL'
--   gives 'Nothing' then tracker do not support scraping.
--
scrapeURL :: URI -> Maybe URI
scrapeURL uri = do
    newPath <- replace (BC.pack (uriPath uri))
    return uri { uriPath = BC.unpack newPath }
  where
    replace p = do
      let ps = BC.splitWith (== '/') p
      guard (not (L.null ps))
      guard ("announce" `BS.isPrefixOf` L.last ps)
      let newSuff = "scrape" <> BS.drop (BS.length "announce") (L.last ps)
      return (BS.intercalate "/" (L.init ps ++ [newSuff]))

-- | For each 'InfoHash' of torrents request scrape info from the tracker.
--   However if the info hash list is 'null', the tracker should list
--   all available torrents.
--
--   This function can throw 'RpcException'.
--
scrape :: Manager -> URI -> ScrapeQuery -> IO ScrapeInfo
scrape m u q = do
  case scrapeURL u of
    Nothing  -> throwIO ScrapelessTracker
    Just uri -> httpTracker m uri (renderScrapeQuery q)

-- | More particular version of 'scrape', just for one torrent.
--
--   This function can throw 'RpcException'.
--
scrapeOne :: Manager -> URI -> InfoHash -> IO ScrapeEntry
scrapeOne m uri ih = do
  xs <- scrape m uri [ih]
  case L.lookup ih xs of
    Nothing -> throwIO BadScrape
    Just a  -> return a
