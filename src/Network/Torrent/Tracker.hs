{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Torrent.Tracker
       ( module Network.Torrent.Tracker.Scrape
       , Event(..), TRequest(..), TResponse(..)
       , defaultRequest, defaultPorts
       , sendRequest
       )
       where

import Network.Torrent.Tracker.Scrape

import Control.Applicative
import Data.Char as Char
import Data.Word (Word32)
import Data.List as L
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
import Network.Torrent.PeerID


data Event = Started   -- ^ For first request.
           | Stopped   -- ^ Sent when the peer is shutting down.
           | Completed -- ^ To be sent when the peer completes a download.
             deriving (Show, Read, Eq, Ord, Enum, Bounded)

data TRequest = TRequest { -- TODO peer here -- TODO detach announce
     reqAnnounce   :: URI         -- ^ Announce url of the torrent.
   , reqInfoHash   :: InfoHash    -- ^ Hash of info part of the torrent.
   , reqPeerID     :: PeerID      -- ^ Id of the peer doing request. ()
   , reqPort       :: PortNumber  -- ^ Port to listen to for connection from other peers.
   , reqUploaded   :: Int         -- ^ # of bytes that the peer has uploaded in the swarm.
   , reqDownloaded :: Int         -- ^ # of bytes downloaded in the swarm by the peer.
   , reqLeft       :: Int         -- ^ # of bytes needed in order to complete download.
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
     , respPeers       :: [Peer]    -- ^ Peers that must be contacted.
     } deriving Show

instance BEncodable PortNumber where
  toBEncode = toBEncode . fromEnum
  fromBEncode b = toEnum <$> fromBEncode b

instance BEncodable Peer where
  toBEncode (Peer pid pip pport) = fromAssocs
    [ "peer id" -->? pid
    , "ip"      -->  pip
    , "port"    -->  pport
    ]

  fromBEncode (BDict d) =
    Peer <$> d >--? "peer id"
         <*> d >--  "ip"
         <*> d >--  "port"

  fromBEncode _ = decodingError "Peer"

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
        getPeers :: Maybe BEncode -> Result [Peer]
        getPeers (Just (BList l))     = fromBEncode (BList l)
        getPeers (Just (BString s))
            | B.length s `mod` 6 == 0 =
              let cnt = B.length s `div` 6 in
              runGet (sequence (L.replicate cnt peerG)) s
            | otherwise = decodingError "peers length not a multiple of 6"
          where
            peerG = do
              pip   <- getWord32be
              pport <- getWord16be
              return (Peer Nothing (fromIntegral pip) (fromIntegral pport))

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

defaultRequest :: URI -> InfoHash -> PeerID -> TRequest
defaultRequest announce hash pid =
  TRequest {
    reqAnnounce = announce
  , reqInfoHash = hash
  , reqPeerID   = pid
  , reqPort     = L.head defaultPorts
  , reqUploaded = 0
  , reqDownloaded = 0
  , reqLeft       = 0
  , reqIP         = Nothing
  , reqNumWant    = Just 25
  , reqEvent      = Just Started
  }

-- | TODO rename to ask for peers
--
sendRequest :: TRequest -> IO (Result TResponse)
sendRequest req = do
  let r = mkHTTPRequest (encodeRequest req)
  print r

  rawResp  <- simpleHTTP r
  respBody <- getResponseBody rawResp
  print respBody
  return (decoded (BC.pack respBody))

  where
    mkHTTPRequest :: URI -> Request String
    mkHTTPRequest uri = Request uri GET [] ""
