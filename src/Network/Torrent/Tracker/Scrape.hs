{-# LANGUAGE OverloadedStrings #-}
module Network.Torrent.Tracker.Scrape
       ( ScrapeInfo(..), Scrape
       , scrapeURL
       ) where

import Control.Applicative
import Data.BEncode
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Torrent.InfoHash
import Network.URI

data ScrapeInfo = ScrapeInfo {
    siComplete   :: Int              -- ^ Number of seeders.
  , siDownloaded :: Int
    -- ^ Total number of times the tracker has registered a completion.
  , siIncomplete :: Int              -- ^ Number of leechers
  , siName       :: Maybe ByteString -- ^
  } deriving (Show, Eq)

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

-- TODO: encode info hash
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
