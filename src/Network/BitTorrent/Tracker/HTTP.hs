-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD
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
--       , scrapeURL
       ) where

import Control.Exception
import Data.BEncode
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.List as L
import Data.Map as M
import Data.Monoid
import Data.URLEncoded as URL
import Network.URI
import Network.HTTP

import Data.Torrent.InfoHash
import Network.BitTorrent.Tracker.Message

-- | Set of tracker RPCs.
class Tracker s where
  connect  :: URI -> IO s
  announce :: s -> AnnounceQuery -> IO AnnounceInfo
  scrape   :: s -> ScrapeQuery   -> IO Scrape

-- | More particular version of 'scrape', just for one torrent.
--
scrapeOne :: Tracker t => t -> InfoHash -> IO ScrapeInfo
scrapeOne uri ih = scrape uri [ih] >>= maybe err return . M.lookup ih
  where
    err = throwIO $ userError "unable to find info hash in response dict"

data HTTPTracker = HTTPTracker
  { announceURI :: URI
  } deriving Show

instance Tracker HTTPTracker where
  connect  = return . HTTPTracker
  announce = announceHTTP
--  scrape   = scrapeHTTP

{-----------------------------------------------------------------------
  Announce
-----------------------------------------------------------------------}

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
    checkResult $ decode respBody
  where
    checkResult (Left err)
      = ioError $ userError $ err ++ " in tracker response"
    checkResult (Right (Failure err))
      = ioError $ userError $ show err ++ " in tracker response"
    checkResult (Right resp)          = return resp
{-
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
    case decode (BC.pack respBody) of
      Left  e -> throwIO $ userError $ e ++ " in scrape response"
      Right r -> return r

  | otherwise = throwIO $ userError "Tracker do not support scraping"
-}