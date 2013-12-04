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
module Network.BitTorrent.Tracker.RPC.HTTP
       ( Connection
       , putConnection

         -- * RPC
       , connect
       , announce
       , scrape
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans.Resource
import Data.BEncode as BE
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy  as BL
import Data.List as L
import Data.Map as M
import Data.Monoid
import Network.URI
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Internal
import Network.HTTP.Types.URI

import Data.Torrent.InfoHash
import Network.BitTorrent.Tracker.Message


data Connection = Connection
  { announceURI :: URI
  , manager     :: Manager
  , connProxy   :: Maybe Proxy
  }

putConnection :: Connection -> IO ()
putConnection = undefined

-- TODO share manager between several threads
connect :: URI -> ResourceT IO Connection
connect uri = do
  (_, m) <- allocate (newManager def) closeManager
  return Connection
    { announceURI = uri
    , manager     = m
    , connProxy   = Nothing
    }

setSimpleQuery :: SimpleQuery -> Request m -> Request m
setSimpleQuery q r = r
  { queryString = undefined renderSimpleQuery False q
  }

trackerHTTP :: BEncode a => SimpleQuery -> Connection -> ResourceT IO a
trackerHTTP q Connection {..} = do
  request  <- setSimpleQuery q <$> setUri def announceURI
  response <- httpLbs request { proxy = connProxy } manager
  case BE.decode $ BL.toStrict $ responseBody response of
    Left  msg  -> error "TODO"
    Right info -> return info

-- | Send request and receive response from the tracker specified in
-- announce list.
--
announce :: AnnounceQuery -> Connection -> ResourceT IO AnnounceInfo
announce q = trackerHTTP (renderAnnounceQuery q)

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
      , "announce" `B.isPrefixOf` L.last ps
      = let newSuff = "scrape" <> B.drop (B.length "announce") (L.last ps)
        in Just (B.intercalate "/" (L.init ps ++ [newSuff]))
      | otherwise = Nothing

-- | For each 'InfoHash' of torrents request scrape info from the tracker.
--   However if the info hash list is 'null', the tracker should list
--   all available torrents.
--
scrape :: ScrapeQuery -> Connection -> ResourceT IO ScrapeInfo
scrape q conn @ Connection {..} = do
  case scrapeURL announceURI of
    Nothing  -> error "Tracker do not support scraping"
    Just uri -> trackerHTTP (renderScrapeQuery q) conn { announceURI = uri }

-- | More particular version of 'scrape', just for one torrent.
--
scrapeOne :: InfoHash -> Connection -> ResourceT IO ScrapeEntry
scrapeOne ih uri = do
  xs <- scrape [ih] uri
  case L.lookup ih xs of
    Nothing -> error "unable to find info hash in response dict"
    Just a  -> return a
