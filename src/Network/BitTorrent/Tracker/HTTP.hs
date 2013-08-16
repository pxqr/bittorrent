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
       ( askTracker, leaveTracker
       , scrapeURL
       ) where

import Control.Applicative
import Control.Monad
import Data.BEncode
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.List as L
import Data.Map as M
import Data.Monoid
import Data.URLEncoded as URL
import Network.URI
import Network.HTTP

import Data.Torrent.Metainfo
import Network.BitTorrent.Tracker.Protocol

{-----------------------------------------------------------------------
  Announce
-----------------------------------------------------------------------}

encodeRequest :: URI -> AnnounceQuery -> URI
encodeRequest announce req = URL.urlEncode req
                    `addToURI`      announce
                    `addHashToURI`  reqInfoHash req

mkHTTPRequest :: URI -> Request ByteString
mkHTTPRequest uri = Request uri GET [] ""

-- TODO rename to something like "announceBlahBlah"

-- | Send request and receive response from the tracker specified in
-- announce list. This function throws 'IOException' if it couldn't
-- send request or receive response or decode response.
--
askTracker :: URI -> AnnounceQuery -> IO AnnounceInfo
askTracker announce req = do
    let r = mkHTTPRequest (encodeRequest announce req)

    rawResp  <- simpleHTTP r
    respBody <- getResponseBody rawResp
    checkResult $ decoded respBody
  where
    checkResult (Left err)
      = ioError $ userError $ err ++ " in tracker response"
    checkResult (Right (Failure err))
      = ioError $ userError $ show err ++ " in tracker response"
    checkResult (Right resp)          = return resp

-- | The same as the 'askTracker' but ignore response. Used in
-- conjunction with 'Stopped'.
leaveTracker :: URI -> AnnounceQuery -> IO ()
leaveTracker announce req = do
  let r = mkHTTPRequest (encodeRequest announce req)
  void $ simpleHTTP r >>= getResponseBody

{-----------------------------------------------------------------------
  Scrape
-----------------------------------------------------------------------}

-- | Scrape info about a set of torrents.
type Scrape = Map InfoHash ScrapeInfo

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
scrape :: URI                -- ^ Announce 'URI'.
       -> [InfoHash]         -- ^ Torrents to be scrapped.
       -> IO (Result Scrape) -- ^ 'ScrapeInfo' for each torrent.
scrape announce ihs
  | Just uri<- scrapeURL announce ihs = do
    rawResp  <- simpleHTTP (Request uri GET [] "")
    respBody <- getResponseBody rawResp
    return (decoded (BC.pack respBody))

  | otherwise = return (Left "Tracker do not support scraping")

-- | More particular version of 'scrape', just for one torrent.
--
scrapeOne :: URI                     -- ^ Announce 'URI'
          -> InfoHash                -- ^ Hash of the torrent info.
          -> IO (Result ScrapeInfo)  -- ^ 'ScrapeInfo' for the torrent.
scrapeOne uri ih = extract <$> scrape uri [ih]
  where
    extract (Right m)
      | Just s <- M.lookup ih m = Right s
      | otherwise = Left "unable to find info hash in response dict"
    extract (Left e) = Left e
