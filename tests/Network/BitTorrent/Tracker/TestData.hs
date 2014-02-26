{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.BitTorrent.Tracker.TestData
       ( TrackerEntry (..)
       , isUdpTracker
       , isHttpTracker
       , trackers
       ) where

import Data.Maybe
import Data.String
import Network.URI

import Data.Torrent.InfoHash


data TrackerEntry = TrackerEntry
  { -- | May be used to show tracker name in test suite report.
    trackerName :: String

    -- | Announce uri of the tracker.
  , trackerURI  :: URI

    -- | Some trackers abadoned, so don't even try to announce.
  , tryAnnounce :: Bool

    -- | Some trackers do not support scraping, so we should not even
    -- try to scrape them.
  , tryScraping :: Bool

    -- | Some trackers allow
  , hashList    :: Maybe [InfoHash]
  }

isUdpTracker :: TrackerEntry -> Bool
isUdpTracker TrackerEntry {..} = uriScheme trackerURI == "udp:"

isHttpTracker :: TrackerEntry -> Bool
isHttpTracker TrackerEntry {..} = uriScheme trackerURI == "http:"
                               || uriScheme trackerURI == "https:"

instance IsString URI where
  fromString str = fromMaybe err $ parseURI str
    where
      err = error $ "fromString: bad URI " ++ show str

trackerEntry :: URI -> TrackerEntry
trackerEntry uri = TrackerEntry
  { trackerName = maybe "<unknown>" uriRegName (uriAuthority uri)
  , trackerURI  = uri
  , tryAnnounce = False
  , tryScraping = False
  , hashList    = Nothing
  }

announceOnly :: String -> URI -> TrackerEntry
announceOnly name uri = (trackerEntry uri)
  { trackerName = name
  , tryAnnounce = True
  }

announceScrape :: String -> URI -> TrackerEntry
announceScrape name uri = (announceOnly name uri)
  { tryScraping = True
  }

notWorking :: String -> URI -> TrackerEntry
notWorking name uri = (trackerEntry uri)
  { trackerName = name
  }

trackers :: [TrackerEntry]
trackers =
  [ announceOnly "LinuxTracker"
                 "http://linuxtracker.org:2710/00000000000000000000000000000000/announce"

  , announceScrape "OpenBitTorrent" "udp://tracker.openbittorrent.com:80/announce"
  , announceScrape "PublicBT"       "udp://tracker.publicbt.com:80/announce"
  , notWorking     "OpenBitTorrent" "http://tracker.openbittorrent.com:80/announce"
  , notWorking     "PublicBT"       "http://tracker.publicbt.com:80/announce"
  ]
