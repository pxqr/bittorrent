-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   By convention most trackers support another form of request,
--   which queries the state of a given torrent (or all torrents) that the
--   tracker is managing. This module provides a way to easily request
--   scrape info for a particular torrent list.
--
{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.Tracker.Scrape
       ( ScrapeInfo(..), Scrape
       , scrapeURL

         -- * Requests
       , scrape
       , scrapeOne
       ) where

import Control.Applicative
import Data.BEncode
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Network.URI
import Network.HTTP

import Data.Torrent


-- | Information about particular torrent.
data ScrapeInfo = ScrapeInfo {
    siComplete   :: Int
    -- ^ Number of seeders - peers with the entire file.
  , siDownloaded :: Int
    -- ^ Total number of times the tracker has registered a completion.
  , siIncomplete :: Int
    -- ^ Number of leechers.
  , siName       :: Maybe ByteString
    -- ^ Name of the torrent file, as specified by the "name"
    --   file in the info section of the .torrent file.
  } deriving (Show, Eq)

-- | Scrape info about a set of torrents.
type Scrape = Map InfoHash ScrapeInfo

instance BEncodable ScrapeInfo where
  toBEncode si = fromAssocs
    [ "complete"   -->  siComplete si
    , "downloaded" -->  siDownloaded si
    , "incomplete" -->  siIncomplete si
    , "name"       -->? siName si
    ]

  fromBEncode (BDict d) =
    ScrapeInfo <$> d >--  "complete"
               <*> d >--  "downloaded"
               <*> d >--  "incomplete"
               <*> d >--? "name"
  fromBEncode _ = decodingError "ScrapeInfo"

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
  return (foldl addHashToURI newURI ihs)
 where
    replace :: ByteString -> Maybe ByteString
    replace p
      | ps <- BC.splitWith (== '/') p
      , "announce" `B.isPrefixOf` last ps
      = let newSuff = "scrape" <> B.drop (B.length "announce") (last ps)
        in Just (B.intercalate "/" (init ps ++ [newSuff]))
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
