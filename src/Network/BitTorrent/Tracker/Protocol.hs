-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
--
--   This module provides straigthforward Tracker protocol
--   implementation. The tracker is an HTTP/HTTPS service:
--
--     * A tracker request is HTTP GET request; used to include
--   metrics from clients that help the tracker keep overall
--   statistics about the torrent.
--
--     * The tracker response includes a peer list that helps the
--   client participate in the torrent.
--
--   For more convenient high level API see Network.BitTorrent.Tracker
--   module.
--
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
-- TODO: add "compact" field to TRequest
module Network.BitTorrent.Tracker.Protocol
       ( module Network.BitTorrent.Tracker.Scrape

       , Event(..), TRequest(..), TResponse(..)
       , sendRequest

         -- * Defaults
       , defaultPorts, defaultNumWant
       )
       where

import Control.Applicative
import Control.Monad
import Data.Char as Char
import Data.Word (Word32)
import Data.Map  as M
import Data.Monoid
import Data.BEncode
import Data.ByteString as B
import           Data.ByteString.Char8 as BC
import           Data.Text as T
import Data.Serialize.Get hiding (Result)
import Data.URLEncoded as URL
import Data.Torrent

import Network
import Network.Socket
import Network.HTTP
import Network.URI

import Network.BitTorrent.Peer
import Network.BitTorrent.Tracker.Scrape


data Event = Started   -- ^ For first request.
           | Stopped   -- ^ Sent when the peer is shutting down.
           | Completed -- ^ To be sent when the peer completes a download.
             deriving (Show, Read, Eq, Ord, Enum, Bounded)

data TRequest = TRequest { -- TODO peer here -- TODO detach announce
     reqAnnounce   :: URI         -- ^ Announce url of the torrent.
   , reqInfoHash   :: InfoHash    -- ^ Hash of info part of the torrent.
   , reqPeerID     :: PeerID      -- ^ Id of the peer doing request. ()
   , reqPort       :: PortNumber  -- ^ Port to listen to for connection from other peers.
   , reqUploaded   :: Integer     -- ^ # of bytes that the peer has uploaded in the swarm.
   , reqDownloaded :: Integer     -- ^ # of bytes downloaded in the swarm by the peer.
   , reqLeft       :: Integer     -- ^ # of bytes needed in order to complete download.
   , reqIP         :: Maybe HostAddress    -- ^ The peer IP.
   , reqNumWant    :: Maybe Int   -- ^ Number of peers that the peers wants to receive from.
   , reqEvent      :: Maybe Event -- ^ If not specified,
                                  --   the request is regular periodic request.
   } deriving Show

data TResponse =
     Failure Text           -- ^ Failure reason in human readable form.
   | OK {
       respWarning     :: Maybe Text
     , respInterval    :: Int       -- ^ Recommended interval to wait between requests.
     , respMinInterval :: Maybe Int -- ^ Minimal amount of time between requests.
     , respComplete    :: Maybe Int -- ^ Number of peers completed the torrent. (seeders)
     , respIncomplete  :: Maybe Int -- ^ Number of peers downloading the torrent.
     , respPeers       :: [PeerAddr]    -- ^ Peers that must be contacted.
     } deriving Show

instance BEncodable TResponse where
  toBEncode (Failure t)  = fromAssocs ["failure reason" --> t]
  toBEncode resp@(OK {}) = fromAssocs
    [ "interval"     -->  respInterval resp
    , "min interval" -->? respMinInterval resp
    , "complete"     -->? respComplete resp
    , "incomplete"   -->? respIncomplete resp
    , "peers"        -->  respPeers resp
    ]

  fromBEncode (BDict d)
    | Just t <- M.lookup "failure reason" d = Failure <$> fromBEncode t
    | otherwise = OK <$> d >--? "warning message"
                     <*> d >--  "interval"
                     <*> d >--? "min interval"
                     <*> d >--? "complete"
                     <*> d >--? "incomplete"
                     <*> getPeers (M.lookup "peers" d)

      where
        getPeers :: Maybe BEncode -> Result [PeerAddr]
        getPeers (Just (BList l))     = fromBEncode (BList l)
        getPeers (Just (BString s))
            | B.length s `mod` 6 == 0 =
              let cnt = B.length s `div` 6 in
              runGet (replicateM cnt peerG) s
            | otherwise = decodingError "peers length not a multiple of 6"
          where
            peerG = do
              pip   <- getWord32be
              pport <- getWord16be
              return (PeerAddr Nothing (fromIntegral pip) (fromIntegral pport))

        getPeers _ = decodingError "Peers"

  fromBEncode _ = decodingError "TResponse"


instance URLShow PortNumber where
  urlShow = urlShow . fromEnum

instance URLShow Word32 where
  urlShow = show

instance URLShow Event where
  urlShow e = urlShow (Char.toLower x : xs)
    where
      -- this is always nonempty list
      (x : xs) = show e

instance URLEncode TRequest where
  urlEncode req = mconcat
      [ s "peer_id"    %=  reqPeerID req
      , s "port"       %=  reqPort req
      , s "uploaded"   %=  reqUploaded req
      , s "downloaded" %=  reqDownloaded req
      , s "left"       %=  reqLeft req
      , s "ip"         %=? reqIP req
      , s "numwant"    %=? reqNumWant req
      , s "event"      %=? reqEvent req
      ]
    where s :: String -> String;  s = id; {-# INLINE s #-}

encodeRequest :: TRequest -> URI
encodeRequest req = URL.urlEncode req
                    `addToURI`      reqAnnounce req
                    `addHashToURI`  reqInfoHash req


-- | Ports typically reserved for bittorrent.
defaultPorts :: [PortNumber]
defaultPorts = [6881..6889]

-- | Above 25, new peers are highly unlikely to increase download speed.
--   Even 30 peers is _plenty_, the official client version 3 in fact only
--   actively forms new connections if it has less than 30 peers and will
--   refuse connections if it has 55. So default value is set to 25.
--
defaultNumWant :: Int
defaultNumWant = 25




-- | TODO rename to ask for peers
--
sendRequest :: TRequest -> IO (Result TResponse)
sendRequest req = do
  let r = mkHTTPRequest (encodeRequest req)

  rawResp  <- simpleHTTP r
  respBody <- getResponseBody rawResp
  return (decoded (BC.pack respBody))

  where
    mkHTTPRequest :: URI -> Request String
    mkHTTPRequest uri = Request uri GET [] ""
