-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Tracker WAI application.
--
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker.Wai
       ( -- * Configuration
         TrackerSettings (..)

         -- * Application
       , tracker
       ) where

import Control.Monad.Trans.Resource
import Data.BEncode as BE
import Data.ByteString
import Data.Default
import Data.List as L
import Network.HTTP.Types
import Network.Wai

import Data.Torrent.Progress
import Network.BitTorrent.Tracker.Message


-- | Various configuration settings used to generate tracker response.
data TrackerSettings = TrackerSettings
  { -- | If peer did not specified the "numwant" then this value is
    -- used.
    defNumWant            :: {-# UNPACK #-} !Int

    -- | If peer specified too big numwant value.
  , maxNumWant            :: {-# UNPACK #-} !Int

    -- | Recommended time interval to wait between regular announce
    -- requests.
  , reannounceInterval    :: {-# UNPACK #-} !Int

    -- | Minimum time interval to wait between regular announce
    -- requests.
  , reannounceMinInterval :: !(Maybe Int)

    -- | Whether to send count of seeders.
  , completePeers         :: !Bool

    -- | Whether to send count of leechers.
  , incompletePeers       :: !Bool

    -- | Do not send peer id in response. Peer can override this value
    -- by setting "no_peer_id" to 0 or 1.
  , noPeerId              :: !Bool

    -- | Whether to send compact peer list. Peer can override this
    -- value by setting "compact" to 0 or 1.
  , compactPeerList       :: !Bool
  } deriving (Show, Read, Eq)

-- | Conservative tracker settings compatible with any client.
instance Default TrackerSettings where
  def = TrackerSettings
    { defNumWant            = defaultNumWant
    , maxNumWant            = defaultMaxNumWant
    , reannounceInterval    = defaultReannounceInterval
    , reannounceMinInterval = Nothing
    , compactPeerList       = False
    , completePeers         = False
    , incompletePeers       = False
    , noPeerId              = False
    }

getAnnounceR :: TrackerSettings -> AnnounceRequest -> ResourceT IO AnnounceInfo
getAnnounceR = undefined

getScrapeR :: TrackerSettings -> ScrapeQuery -> ResourceT IO ScrapeInfo
getScrapeR = undefined

-- content-type: "text/plain"!
tracker :: TrackerSettings -> Application
tracker settings Request {..}
  | requestMethod /= methodGet
  = return $ responseLBS methodNotAllowed405 [] ""

  | otherwise = do
    case pathInfo of
      ["announce"] ->
        case parseAnnounceRequest $ queryToSimpleQuery queryString of
          Right query -> do
            info <- getAnnounceR settings query
            return $ responseLBS ok200 [] $ BE.encode info
          Left msg ->
            return $ responseLBS (parseFailureStatus msg) [] ""

      ["scrape"]   ->
        case Right $ parseScrapeQuery $ queryToSimpleQuery queryString of -- TODO
          Right query -> do
            info <- getScrapeR settings query
            return $ responseLBS ok200 [] $ BE.encode info
          Left _ ->
            return $ responseLBS badRequest400 [] ""

      _            -> undefined --badPath
