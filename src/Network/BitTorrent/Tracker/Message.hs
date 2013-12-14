-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Every tracker should support announce query. This query is used
--   to discover peers within a swarm and have two-fold effect:
--
--     * peer doing announce discover other peers using peer list from
--     the response to the announce query.
--
--     * tracker store peer information and use it in the succeeding
--     requests made by other peers, until the peer info expires.
--
--   By convention most trackers support another form of request —
--   scrape query — which queries the state of a given torrent (or
--   a list of torrents) that the tracker is managing.
--
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -fno-warn-orphans           #-}
module Network.BitTorrent.Tracker.Message
       ( -- * Announce
         -- ** Query
         Event(..)
       , AnnounceQuery(..)
       , renderAnnounceQuery
       , ParamParseFailure
       , parseAnnounceQuery

         -- ** Request
       , AnnounceQueryExt (..)
       , renderAnnounceQueryExt
       , parseAnnounceQueryExt

       , AnnounceRequest  (..)
       , parseAnnounceRequest
       , renderAnnounceRequest

         -- ** Info
       , PeerList (..)
       , AnnounceInfo(..)
       , defaultNumWant
       , defaultMaxNumWant
       , defaultReannounceInterval
       , announceType
       , parseFailureStatus

         -- * Scrape
         -- ** Query
       , ScrapeQuery
       , renderScrapeQuery
       , parseScrapeQuery

         -- ** Info
       , ScrapeEntry (..)
       , ScrapeInfo

         -- * Extra
       , queryToSimpleQuery
       )
       where

import Control.Applicative
import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.TH
import Data.BEncode as BE hiding (Result)
import Data.BEncode.BDict as BE
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.Char as Char
import Data.Convertible
import Data.Default
import Data.List as L
import Data.Maybe
import Data.Serialize as S hiding (Result)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Typeable
import Data.Word
import Data.IP
import Network
import Network.HTTP.Types.QueryLike
import Network.HTTP.Types.URI hiding (urlEncode)
import Network.HTTP.Types.Status
import Network.Socket
import Text.Read (readMaybe)

import Data.Torrent.InfoHash
import Data.Torrent.JSON
import Data.Torrent.Progress
import Network.BitTorrent.Core


{-----------------------------------------------------------------------
--  Events
-----------------------------------------------------------------------}

-- | Events used to specify which kind of announce query is performed.
data Event = Started
             -- ^ For the first request: when a peer join the swarm.
           | Stopped
             -- ^ Sent when the peer is shutting down.
           | Completed
             -- ^ To be sent when the peer completes a download.
             deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

$(deriveJSON omitRecordPrefix ''Event)

-- | HTTP tracker protocol compatible encoding.
instance QueryValueLike Event where
  toQueryValue e = toQueryValue (Char.toLower x : xs)
    where
      (x : xs) = show e -- INVARIANT: this is always nonempty list

type EventId = Word32

-- | UDP tracker encoding event codes.
eventId :: Event -> EventId
eventId Completed = 1
eventId Started   = 2
eventId Stopped   = 3

-- TODO add Regular event
putEvent :: Putter (Maybe Event)
putEvent Nothing  = putWord32be 0
putEvent (Just e) = putWord32be (eventId e)

getEvent :: S.Get (Maybe Event)
getEvent = do
  eid <- getWord32be
  case eid of
    0 -> return Nothing
    1 -> return $ Just Completed
    2 -> return $ Just Started
    3 -> return $ Just Stopped
    _ -> fail "unknown event id"

{-----------------------------------------------------------------------
  Announce query
-----------------------------------------------------------------------}

-- | A tracker request is HTTP GET request; used to include metrics
--   from clients that help the tracker keep overall statistics about
--   the torrent. The most important, requests are used by the tracker
--   to keep track lists of active peer for a particular torrent.
--
data AnnounceQuery = AnnounceQuery
   {
     -- | Hash of info part of the torrent usually obtained from
     -- 'Torrent' or 'Magnet'.
     reqInfoHash   :: !InfoHash

     -- | ID of the peer doing request.
   , reqPeerId     :: !PeerId

     -- | Port to listen to for connections from other
     -- peers. Tracker should respond with this port when
     -- some /other/ peer request the tracker with the same info hash.
     -- Normally, this port is choosed from 'defaultPorts'.
   , reqPort       :: !PortNumber

     -- | Current progress of peer doing request.
   , reqProgress   :: !Progress

     -- | The peer IP. Needed only when client communicated with
     -- tracker throught a proxy.
   , reqIP         :: Maybe HostAddress

     -- | Number of peers that the peers wants to receive from. See
     -- note for 'defaultNumWant'.
   , reqNumWant    :: Maybe Int

     -- | If not specified, the request is regular periodic request.
   , reqEvent      :: Maybe Event
   } deriving (Show, Eq, Typeable)

$(deriveJSON omitRecordPrefix ''AnnounceQuery)

-- | UDP tracker protocol compatible encoding.
instance Serialize AnnounceQuery where
  put AnnounceQuery {..} = do
    put           reqInfoHash
    put           reqPeerId
    put           reqProgress
    putEvent      reqEvent
    putWord32be $ fromMaybe 0 reqIP
    putWord32be $ 0 -- TODO what the fuck is "key"?
    putWord32be $ fromIntegral $ fromMaybe (-1) reqNumWant

    put           reqPort

  get = do
    ih   <- get
    pid  <- get

    progress <- get

    ev   <- getEvent
    ip   <- getWord32be
--    key  <- getWord32be -- TODO
    want <- getWord32be

    port <- get

    return $ AnnounceQuery {
        reqInfoHash   = ih
      , reqPeerId     = pid
      , reqPort       = port
      , reqProgress   = progress
      , reqIP         = if ip == 0 then Nothing else Just ip
      , reqNumWant    = if want == -1 then Nothing
                        else Just (fromIntegral want)
      , reqEvent      = ev
      }

instance QueryValueLike PortNumber where
  toQueryValue = toQueryValue . show . fromEnum

instance QueryValueLike Word32 where
  toQueryValue = toQueryValue . show

instance QueryValueLike Int where
  toQueryValue = toQueryValue . show

-- | HTTP tracker protocol compatible encoding.
instance QueryLike AnnounceQuery where
  toQuery AnnounceQuery {..} =
      toQuery reqProgress ++
      [ ("info_hash", toQueryValue reqInfoHash) -- TODO use 'paramName'
      , ("peer_id"  , toQueryValue reqPeerId)
      , ("port"     , toQueryValue reqPort)
      , ("ip"       , toQueryValue reqIP)
      , ("numwant"  , toQueryValue reqNumWant)
      , ("event"    , toQueryValue reqEvent)
      ]

-- | Filter @param=value@ pairs with the unset value.
queryToSimpleQuery :: Query -> SimpleQuery
queryToSimpleQuery = catMaybes . L.map f
  where
    f (_, Nothing) = Nothing
    f (a, Just b ) = Just (a, b)

-- | Encode announce query to query string.
renderAnnounceQuery :: AnnounceQuery -> SimpleQuery
renderAnnounceQuery = queryToSimpleQuery . toQuery

data QueryParam
    -- announce query
  = ParamInfoHash
  | ParamPeerId
  | ParamPort
  | ParamUploaded
  | ParamLeft
  | ParamDownloaded
  | ParamIP
  | ParamNumWant
  | ParamEvent
    -- announce query ext
  | ParamCompact
  | ParamNoPeerId
    deriving (Show, Eq, Ord, Enum)

paramName :: QueryParam -> BS.ByteString
paramName ParamInfoHash   = "info_hash"
paramName ParamPeerId     = "peer_id"
paramName ParamPort       = "port"
paramName ParamUploaded   = "uploaded"
paramName ParamLeft       = "left"
paramName ParamDownloaded = "downloaded"
paramName ParamIP         = "ip"
paramName ParamNumWant    = "numwant"
paramName ParamEvent      = "event"
paramName ParamCompact    = "compact"
paramName ParamNoPeerId   = "no_peer_id"
{-# INLINE paramName #-}

class FromParam a where
  fromParam :: BS.ByteString -> Maybe a

instance FromParam InfoHash where
  fromParam = either (const Nothing) pure . safeConvert

instance FromParam PeerId where
  fromParam = either (const Nothing) pure . safeConvert

instance FromParam Word32 where
  fromParam = readMaybe . BC.unpack

instance FromParam Word64 where
  fromParam = readMaybe . BC.unpack

instance FromParam Int where
  fromParam = readMaybe . BC.unpack

instance FromParam PortNumber where
  fromParam bs = fromIntegral <$> (fromParam bs :: Maybe Word32)

instance FromParam Event where
  fromParam bs = do
    (x, xs) <- BC.uncons bs
    readMaybe $ BC.unpack $ BC.cons (Char.toUpper x) xs

-- | 'ParamParseFailure' represent errors can occur while parsing HTTP
-- tracker requests. In case of failure, this can be used to provide
-- more informative 'statusCode' and 'statusMessage' in tracker
-- responses.
--
data ParamParseFailure
  = Missing QueryParam               -- ^ param not found in query string;
  | Invalid QueryParam BS.ByteString -- ^ param present but not valid.
    deriving (Show, Eq)

type ParseResult = Either ParamParseFailure

withError :: ParamParseFailure -> Maybe a -> ParseResult a
withError e = maybe (Left e) Right

reqParam :: FromParam a => QueryParam -> SimpleQuery -> ParseResult a
reqParam param xs = do
  val <- withError (Missing param) $ L.lookup (paramName param) xs
  withError (Invalid param val) (fromParam val)

optParam :: FromParam a => QueryParam -> SimpleQuery -> ParseResult (Maybe a)
optParam param ps
  | Just x <- L.lookup (paramName param) ps
  = pure <$> withError (Invalid param x) (fromParam x)
  | otherwise = pure Nothing

parseProgress :: SimpleQuery -> ParseResult Progress
parseProgress params = Progress
  <$> reqParam ParamDownloaded params
  <*> reqParam ParamLeft       params
  <*> reqParam ParamUploaded   params

-- | Parse announce request from a query string.
parseAnnounceQuery :: SimpleQuery -> ParseResult AnnounceQuery
parseAnnounceQuery params = AnnounceQuery
  <$> reqParam ParamInfoHash params
  <*> reqParam ParamPeerId   params
  <*> reqParam ParamPort     params
  <*> parseProgress params
  <*> optParam ParamIP       params
  <*> optParam ParamNumWant  params
  <*> optParam ParamEvent    params

-- | Extensions for HTTP tracker protocol.
data AnnounceQueryExt = AnnounceQueryExt
  { -- | If specified, "compact" parameter is used to advise the
    --   tracker to send peer id list as:
    --
    --   * bencoded list                 (extCompact = Just False);
    --   * or more compact binary string (extCompact = Just True).
    --
    --   The later is prefered since compact peer list will reduce the
    --   size of tracker responses. Hovewer, if tracker do not support
    --   this extension then it can return peer list in either form.
    --
    --   For more info see: <http://www.bittorrent.org/beps/bep_0023.html>
    --
    extCompact  :: !(Maybe Bool)

    -- | If specified, "no_peer_id" parameter is used advise tracker
    --   to either send or not to send peer id in tracker response.
    --   Tracker may not support this extension as well.
    --
    --   For more info see:
    --  <http://permalink.gmane.org/gmane.network.bit-torrent.general/4030>
    --
  , extNoPeerId :: !(Maybe Bool)
  } deriving (Show, Eq, Typeable)

instance Default AnnounceQueryExt where
  def = AnnounceQueryExt Nothing Nothing

instance QueryLike AnnounceQueryExt where
  toQuery AnnounceQueryExt {..} =
      [ ("compact",    toQueryFlag <$> extCompact) -- TODO use 'paramName'
      , ("no_peer_id", toQueryFlag <$> extNoPeerId)
      ]
    where
      toQueryFlag False = "0"
      toQueryFlag True  = "1"

instance FromParam Bool where
  fromParam "0" = Just False
  fromParam "1" = Just True
  fromParam _   = Nothing

-- | Parse announce query extended part from query string.
parseAnnounceQueryExt :: SimpleQuery -> AnnounceQueryExt
parseAnnounceQueryExt params = either (const def) id $
  AnnounceQueryExt
    <$> optParam ParamCompact  params
    <*> optParam ParamNoPeerId params

-- | Render announce query extended part to query string.
renderAnnounceQueryExt :: AnnounceQueryExt -> SimpleQuery
renderAnnounceQueryExt = queryToSimpleQuery . toQuery

-- | HTTP tracker request with extensions.
data AnnounceRequest = AnnounceRequest
  { announceQuery   :: AnnounceQuery    -- ^ Request query params.
  , announceAdvises :: AnnounceQueryExt -- ^ Optional advises to the tracker.
  } deriving (Show, Eq, Typeable)

instance QueryLike AnnounceRequest where
  toQuery AnnounceRequest{..} = toQuery announceAdvises ++ toQuery announceQuery

-- | Parse announce request from query string.
parseAnnounceRequest :: SimpleQuery -> ParseResult AnnounceRequest
parseAnnounceRequest params = AnnounceRequest
  <$> parseAnnounceQuery params
  <*> pure (parseAnnounceQueryExt params)

-- | Render announce request to query string.
renderAnnounceRequest :: AnnounceRequest -> SimpleQuery
renderAnnounceRequest = queryToSimpleQuery . toQuery

{-----------------------------------------------------------------------
--  Announce response
-----------------------------------------------------------------------}

-- | Tracker can return peer list in either compact(BEP23) or not
-- compact form.
--
--   For more info see: <http://www.bittorrent.org/beps/bep_0023.html>
--
data PeerList a
  =        PeerList { getPeerList :: [PeerAddr a] }
  | CompactPeerList { getPeerList :: [PeerAddr a] }
    deriving (Show, Eq, Typeable, Functor)

putCompactPeerList :: (Serialize a) => S.Putter [PeerAddr a]
putCompactPeerList = mapM_ put

getCompactPeerList :: (Serialize a) => S.Get [PeerAddr a]
getCompactPeerList = many get

instance (Typeable a, BEncode a, Serialize a) => BEncode (PeerList a) where
  toBEncode (PeerList        xs) = toBEncode xs
  toBEncode (CompactPeerList xs) = toBEncode $ runPut (putCompactPeerList xs)

  fromBEncode (BList    l ) = PeerList        <$> fromBEncode (BList l)
  fromBEncode (BString  s ) = CompactPeerList <$> runGet getCompactPeerList s
  fromBEncode  _            = decodingError "Peer list"

-- | The tracker response includes a peer list that helps the client
--   participate in the torrent. The most important is 'respPeer' list
--   used to join the swarm.
--
data AnnounceInfo =
     Failure !Text -- ^ Failure reason in human readable form.
   | AnnounceInfo {
       -- | Number of peers completed the torrent. (seeders)
       respComplete    :: !(Maybe Int)

       -- | Number of peers downloading the torrent. (leechers)
     , respIncomplete  :: !(Maybe Int)

       -- | Recommended interval to wait between requests, in seconds.
     , respInterval    :: !Int

       -- | Minimal amount of time between requests, in seconds. A
       -- peer /should/ make timeout with at least 'respMinInterval'
       -- value, otherwise tracker might not respond. If not specified
       -- the same applies to 'respInterval'.
     , respMinInterval :: !(Maybe Int)

       -- | Peers that must be contacted.
     , respPeers       :: !(PeerList IP)

       -- | Human readable warning.
     , respWarning     :: !(Maybe Text)
     } deriving (Show, Typeable)

-- | HTTP tracker protocol compatible encoding.
instance BEncode AnnounceInfo where
  toBEncode (Failure t)        = toDict $
       "failure reason" .=! t
    .: endDict

  toBEncode  AnnounceInfo {..} = toDict $
       "complete"        .=? respComplete
    .: "incomplete"      .=? respIncomplete
    .: "interval"        .=! respInterval
    .: "min interval"    .=? respMinInterval
    .: "peers"           .=! peers
    .: "peers6"          .=! peers6
    .: "warning message" .=? respWarning
    .: endDict
        where (peers,peers6) = splitIPList $ getPeerList respPeers

  fromBEncode (BDict d)
    | Just t <- BE.lookup "failure reason" d = Failure <$> fromBEncode t
    | otherwise = (`fromDict` (BDict d)) $
       AnnounceInfo
        <$>? "complete"
        <*>? "incomplete"
        <*>! "interval"
        <*>? "min interval"
        <*>  (PeerList <$> (mergeIPLists <$>! "peers" <*>? "peers6"))
        <*>? "warning message"
  fromBEncode _ = decodingError "Announce info"

-- | UDP tracker protocol compatible encoding.
instance Serialize AnnounceInfo where
  put (Failure msg) = put $ encodeUtf8 msg
  put  AnnounceInfo {..} = do
    putWord32be $ fromIntegral respInterval
    putWord32be $ fromIntegral $ fromMaybe 0 respIncomplete
    putWord32be $ fromIntegral $ fromMaybe 0 respComplete
    forM_ (fmap ipv4 <$> getPeerList respPeers) put

  get = do
    interval <- getWord32be
    leechers <- getWord32be
    seeders  <- getWord32be
    peers    <- many $ fmap IPv4 <$> get

    return $ AnnounceInfo {
        respWarning     = Nothing
      , respInterval    = fromIntegral interval
      , respMinInterval = Nothing
      , respIncomplete  = Just $ fromIntegral leechers
      , respComplete    = Just $ fromIntegral seeders
      , respPeers       = PeerList peers
      }

-- | Above 25, new peers are highly unlikely to increase download
--   speed.  Even 30 peers is /plenty/, the official client version 3
--   in fact only actively forms new connections if it has less than
--   30 peers and will refuse connections if it has 55.
--
--   <https://wiki.theory.org/BitTorrent_Tracker_Protocol#Basic_Tracker_Announce_Request>
--
defaultNumWant :: Int
defaultNumWant = 50

-- | Reasonable upper bound of numwant parameter.
defaultMaxNumWant :: Int
defaultMaxNumWant = 200

-- | Widely used reannounce interval. Note: tracker clients should not
-- use this value!
defaultReannounceInterval :: Int
defaultReannounceInterval = 30 * 60

missingOffset :: Int
missingOffset = 101

invalidOffset :: Int
invalidOffset = 150

parseFailureCode :: ParamParseFailure -> Int
parseFailureCode (Missing param  ) = missingOffset + fromEnum param
parseFailureCode (Invalid param _) = invalidOffset + fromEnum param

parseFailureMessage :: ParamParseFailure -> BS.ByteString
parseFailureMessage e = BS.concat $ case e of
  Missing p   -> ["Missing parameter: ", paramName p]
  Invalid p v -> ["Invalid parameter: ", paramName p, " = ", v]

-- | HTTP response /content type/.
announceType :: ByteString
announceType = "text/plain"

-- | Get HTTP response status from a announce params parse failure.
--
--   For more info see:
--   <https://wiki.theory.org/BitTorrent_Tracker_Protocol#Response_Codes>
--
parseFailureStatus :: ParamParseFailure -> Status
parseFailureStatus = mkStatus <$> parseFailureCode <*> parseFailureMessage

{-----------------------------------------------------------------------
  Scrape message
-----------------------------------------------------------------------}

-- | Scrape query used to specify a set of torrent to scrape.
-- If list is empty then tracker should return scrape info about each
-- torrent.
type ScrapeQuery = [InfoHash]

-- TODO
-- data ScrapeQuery
--  = ScrapeAll
--  | ScrapeSingle InfoHash
--  | ScrapeMulti (HashSet InfoHash)
--    deriving (Show)
--
--  data ScrapeInfo
--    = ScrapeAll   (HashMap InfoHash ScrapeEntry)
--    | ScrapeSingle InfoHash ScrapeEntry
--    | ScrapeMulti (HashMap InfoHash ScrapeEntry)
--

scrapeParam :: BS.ByteString
scrapeParam = "info_hash"

isScrapeParam :: BS.ByteString -> Bool
isScrapeParam = (==) scrapeParam

-- | Parse scrape query to query string.
parseScrapeQuery :: SimpleQuery -> ScrapeQuery
parseScrapeQuery
  = catMaybes . L.map (fromParam . snd) . L.filter (isScrapeParam . fst)

-- | Render scrape query to query string.
renderScrapeQuery :: ScrapeQuery -> SimpleQuery
renderScrapeQuery = queryToSimpleQuery . L.map mkPair
  where
    mkPair ih = (scrapeParam, toQueryValue ih)

-- | Overall information about particular torrent.
data ScrapeEntry = ScrapeEntry {
    -- | Number of seeders - peers with the entire file.
    siComplete   :: {-# UNPACK #-} !Int

    -- | Total number of times the tracker has registered a completion.
  , siDownloaded :: {-# UNPACK #-} !Int

    -- | Number of leechers.
  , siIncomplete :: {-# UNPACK #-} !Int

    -- | Name of the torrent file, as specified by the "name"
    --   file in the info section of the .torrent file.
  , siName       :: !(Maybe Text)
  } deriving (Show, Eq, Typeable)

$(deriveJSON omitRecordPrefix ''ScrapeEntry)

-- | HTTP tracker protocol compatible encoding.
instance BEncode ScrapeEntry where
  toBEncode ScrapeEntry {..} = toDict $
       "complete"   .=! siComplete
    .: "downloaded" .=! siDownloaded
    .: "incomplete" .=! siIncomplete
    .: "name"       .=? siName
    .: endDict

  fromBEncode = fromDict $ ScrapeEntry
    <$>! "complete"
    <*>! "downloaded"
    <*>! "incomplete"
    <*>? "name"

-- | UDP tracker protocol compatible encoding.
instance Serialize ScrapeEntry where
  put ScrapeEntry {..} = do
    putWord32be $ fromIntegral siComplete
    putWord32be $ fromIntegral siDownloaded
    putWord32be $ fromIntegral siIncomplete

  get = ScrapeEntry
    <$> (fromIntegral <$> getWord32be)
    <*> (fromIntegral <$> getWord32be)
    <*> (fromIntegral <$> getWord32be)
    <*> pure Nothing

-- | Scrape info about a set of torrents.
type ScrapeInfo = [(InfoHash, ScrapeEntry)]
