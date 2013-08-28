-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
--   The tracker is an HTTP/HTTPS service used to discovery peers for
--   a particular existing torrent and keep statistics about the
--   swarm. This module also provides a way to easily request scrape
--   info for a particular torrent list.
--
--   For more information see:
--   <https://wiki.theory.org/BitTorrentSpecification#Tracker_HTTP.2FHTTPS_Protocol>
--
module Network.BitTorrent.Tracker.HTTP
       ( HTTPTracker

         -- * Extra
       , scrapeURL
       ) where

import Control.Exception
import Data.BEncode
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.List as L
import Data.Monoid
import Data.URLEncoded as URL
import Network.URI
import Network.HTTP

import Data.Torrent.Metainfo hiding (announce)
import Network.BitTorrent.Tracker.Protocol


data HTTPTracker = HTTPTracker
  { announceURI :: URI
  } deriving Show

instance Tracker HTTPTracker where
  connect  = return . HTTPTracker
  announce = announceHTTP
  scrape   = scrapeHTTP

{-----------------------------------------------------------------------
  Announce
-----------------------------------------------------------------------}

encodeRequest :: URI -> AnnounceQuery -> URI
encodeRequest announceURI req = URL.urlEncode req
                    `addToURI`      announceURI
                    `addHashToURI`  reqInfoHash req

mkGET :: URI -> Request ByteString
mkGET uri = Request uri GET [] ""

-- | Send request and receive response from the tracker specified in
-- announce list. This function throws 'IOException' if it couldn't
-- send request or receive response or decode response.
--
announceHTTP :: HTTPTracker -> AnnounceQuery -> IO AnnounceInfo
announceHTTP HTTPTracker {..} req = do
    let r = mkGET (encodeRequest announceURI req)

    rawResp  <- simpleHTTP r
    respBody <- getResponseBody rawResp
    checkResult $ decoded respBody
  where
    checkResult (Left err)
      = ioError $ userError $ err ++ " in tracker response"
    checkResult (Right (Failure err))
      = ioError $ userError $ show err ++ " in tracker response"
    checkResult (Right resp)          = return resp

{-----------------------------------------------------------------------
  Scrape
-----------------------------------------------------------------------}

-- | Trying to convert /announce/ URL to /scrape/ URL. If 'scrapeURL'
--   gives 'Nothing' then tracker do not support scraping. The info hash
--   list is used to restrict the tracker's report to that particular
--   torrents. Note that scrapping of multiple torrents may not be
--   supported. (Even if scrapping convention is supported)
--
scrapeURL :: URI -> [InfoHash] -> Maybe URI
scrapeURL uri ihs = do
  newPath <- replace (BC.pack (uriPath uri))
  let newURI = uri { uriPath = BC.unpack newPath }
  return (L.foldl addHashToURI newURI ihs)
 where
    replace :: ByteString -> Maybe ByteString
    replace p
      | ps <- BC.splitWith (== '/') p
      , "announce" `B.isPrefixOf` L.last ps
      = let newSuff = "scrape" <> B.drop (B.length "announce") (L.last ps)
        in Just (B.intercalate "/" (L.init ps ++ [newSuff]))
      | otherwise = Nothing


-- | For each 'InfoHash' of torrents request scrape info from the tracker.
--   However if the info hash list is 'null', the tracker should list
--   all available torrents.
--   Note that the 'URI' should be /announce/ URI, not /scrape/ URI.
--
scrapeHTTP :: HTTPTracker -- ^ Announce 'URI'.
           -> [InfoHash]  -- ^ Torrents to be scrapped.
           -> IO Scrape   -- ^ 'ScrapeInfo' for each torrent.
scrapeHTTP HTTPTracker {..} ihs
  | Just uri <- scrapeURL announceURI ihs = do
    rawResp  <- simpleHTTP (Request uri GET [] "")
    respBody <- getResponseBody rawResp
    case decoded (BC.pack respBody) of
      Left  e -> throwIO $ userError $ e ++ " in scrape response"
      Right r -> return r

  | otherwise = throwIO $ userError "Tracker do not support scraping"
