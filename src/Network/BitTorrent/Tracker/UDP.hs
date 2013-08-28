-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module implement low-level UDP tracker protocol.
--   For more info see:
--   <http://www.bittorrent.org/beps/bep_0015.html>
--
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Network.BitTorrent.Tracker.UDP
       ( UDPTracker

         -- * Debug
       , putTracker
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.Text as T
import Data.Text.Encoding
import Data.Time
import Data.Word
import Text.Read (readMaybe)
import Network.Socket hiding (Connected)
import Network.Socket.ByteString as BS
import Network.URI
import System.Entropy
import System.Timeout
import Numeric

import Data.Torrent.Metainfo ()
import Network.BitTorrent.Tracker.Protocol

{-----------------------------------------------------------------------
  Tokens
-----------------------------------------------------------------------}

genToken :: IO Word64
genToken = do
    bs <- getEntropy 8
    either err return $ runGet getWord64be bs
  where
    err = error "genToken: impossible happen"

-- | Connection Id is used for entire tracker session.
newtype ConnectionId  = ConnectionId Word64
                        deriving (Eq, Serialize)

instance Show ConnectionId where
  showsPrec _ (ConnectionId cid) = showString "0x" <> showHex cid

genConnectionId :: IO ConnectionId
genConnectionId = ConnectionId <$> genToken

initialConnectionId :: ConnectionId
initialConnectionId =  ConnectionId 0x41727101980

-- TODO rename
-- | Transaction Id is used within a UDP RPC.
newtype TransactionId = TransactionId Word32
                        deriving (Eq, Serialize)

instance Show TransactionId where
  showsPrec _ (TransactionId tid) = showString "0x" <> showHex tid

genTransactionId :: IO TransactionId
genTransactionId = (TransactionId . fromIntegral) <$> genToken

{-----------------------------------------------------------------------
  Transactions
-----------------------------------------------------------------------}

data Request  = Connect
              | Announce  AnnounceQuery
              | Scrape    ScrapeQuery
                deriving Show

data Response = Connected ConnectionId
              | Announced AnnounceInfo
              | Scraped   [ScrapeInfo]
              | Failed    Text
                deriving Show

data family Transaction a
data instance Transaction Request  = TransactionQ
    { connIdQ  :: {-# UNPACK #-} !ConnectionId
    , transIdQ :: {-# UNPACK #-} !TransactionId
    , request  :: !Request
    } deriving Show
data instance Transaction Response = TransactionR
    { transIdR :: {-# UNPACK #-} !TransactionId
    , response :: !Response
    } deriving Show

-- TODO newtype
newtype MessageId = MessageId Word32
                    deriving (Show, Eq, Num, Serialize)

connectId, announceId, scrapeId, errorId :: MessageId
connectId  = 0
announceId = 1
scrapeId   = 2
errorId    = 3

instance Serialize (Transaction Request) where
  put TransactionQ {..} = do
    case request of
      Connect        -> do
        put initialConnectionId
        put connectId
        put transIdQ

      Announce ann -> do
        put connIdQ
        put announceId
        put transIdQ
        put ann

      Scrape   hashes -> do
        put connIdQ
        put scrapeId
        put transIdQ
        forM_ hashes put

  get = do
      cid <- get
      mid <- get
      TransactionQ cid <$> get <*> getBody mid
    where
      getBody :: MessageId -> Get Request
      getBody msgId
        | msgId == connectId  = pure Connect
        | msgId == announceId = Announce <$> get
        | msgId == scrapeId   = Scrape   <$> many get
        |       otherwise     = fail errMsg
        where
          errMsg = "unknown request: " ++ show msgId

instance Serialize (Transaction Response) where
  put TransactionR {..} = do
    case response of
      Connected conn -> do
        put connectId
        put transIdR
        put conn

      Announced info -> do
        put announceId
        put transIdR
        put info

      Scraped infos -> do
        put scrapeId
        put transIdR
        forM_ infos put

      Failed info -> do
        put errorId
        put transIdR
        put (encodeUtf8 info)


  get = do
      mid <- get
      TransactionR <$> get <*> getBody mid
    where
      getBody :: MessageId -> Get Response
      getBody msgId
        | msgId == connectId  = Connected <$> get
        | msgId == announceId = Announced <$> get
        | msgId == scrapeId   = Scraped   <$> many get
        | msgId == errorId    = (Failed . decodeUtf8) <$> get
        |       otherwise     = fail msg
        where
          msg = "unknown response: " ++ show msgId

{-----------------------------------------------------------------------
  Connection
-----------------------------------------------------------------------}

connectionLifetime :: NominalDiffTime
connectionLifetime = 60

connectionLifetimeServer :: NominalDiffTime
connectionLifetimeServer = 120

data Connection = Connection
    { connectionId        :: ConnectionId
    , connectionTimestamp :: UTCTime
    } deriving Show

initialConnection :: IO Connection
initialConnection = Connection initialConnectionId <$> getCurrentTime

isExpired :: Connection -> IO Bool
isExpired Connection {..} = do
  currentTime <- getCurrentTime
  let timeDiff = diffUTCTime currentTime connectionTimestamp
  return $ timeDiff > connectionLifetime

{-----------------------------------------------------------------------
  RPC
-----------------------------------------------------------------------}

maxPacketSize :: Int
maxPacketSize = 98 -- announce request packet

setPort :: PortNumber -> SockAddr -> SockAddr
setPort p (SockAddrInet  _ h)     = SockAddrInet  p h
setPort p (SockAddrInet6 _ f h s) = SockAddrInet6 p f h s
setPort _  addr = addr

getTrackerAddr :: URI -> IO SockAddr
getTrackerAddr URI { uriAuthority = Just (URIAuth {..}) } = do
  infos <- getAddrInfo Nothing (Just uriRegName) Nothing
  let port = fromMaybe 0 (readMaybe (L.drop 1 uriPort) :: Maybe Int)
  case infos of
    AddrInfo {..} : _ -> return $ setPort (fromIntegral port) addrAddress
    _                 -> fail "getTrackerAddr: unable to lookup host addr"
getTrackerAddr _       = fail "getTrackerAddr: hostname unknown"

call :: SockAddr -> ByteString -> IO ByteString
call addr arg = bracket open close rpc
  where
    open = socket AF_INET Datagram defaultProtocol
    rpc sock = do
      BS.sendAllTo sock arg addr
      (res, addr') <- BS.recvFrom sock maxPacketSize
      unless (addr' == addr) $ do
        throwIO $ userError "address mismatch"
      return res

-- TODO retransmissions
-- TODO blocking
data UDPTracker = UDPTracker
    { trackerURI        :: URI
    , trackerConnection :: IORef Connection
    }

updateConnection :: ConnectionId -> UDPTracker -> IO ()
updateConnection cid UDPTracker {..} = do
  newConnection <- Connection cid <$> getCurrentTime
  writeIORef trackerConnection newConnection

getConnectionId :: UDPTracker -> IO ConnectionId
getConnectionId UDPTracker {..}
  = connectionId <$> readIORef trackerConnection

putTracker :: UDPTracker -> IO ()
putTracker UDPTracker {..} = do
  print trackerURI
  print =<< readIORef trackerConnection

transaction :: UDPTracker -> Request -> IO Response
transaction tracker @ UDPTracker {..} request = do
  cid <- getConnectionId tracker
  tid <- genTransactionId
  let trans = TransactionQ cid tid request

  addr <- getTrackerAddr trackerURI
  res  <- call addr (encode trans)
  case decode res of
    Right (TransactionR {..})
      | tid == transIdR -> return response
      |   otherwise     -> throwIO $ userError "transaction id mismatch"
    Left msg            -> throwIO $ userError msg

connectUDP :: UDPTracker -> IO ConnectionId
connectUDP tracker = do
  resp <- transaction tracker Connect
  case resp of
    Connected cid -> return cid
    Failed    msg -> throwIO $ userError $ T.unpack msg
    _             -> throwIO $ userError "message type mismatch"

initialTracker :: URI -> IO UDPTracker
initialTracker uri = do
  tracker <- UDPTracker uri <$> (newIORef =<< initialConnection)
  connId  <- connectUDP tracker
  updateConnection connId tracker
  return tracker

freshConnection :: UDPTracker -> IO ()
freshConnection tracker @ UDPTracker {..} = do
  conn    <- readIORef trackerConnection
  expired <- isExpired conn
  when expired $ do
    connId <- connectUDP tracker
    updateConnection connId tracker

announceUDP :: UDPTracker -> AnnounceQuery -> IO AnnounceInfo
announceUDP tracker ann = do
  freshConnection tracker
  resp <- transaction tracker (Announce ann)
  case resp of
    Announced info -> return info
    _              -> fail "announce: response type mismatch"

scrapeUDP :: UDPTracker -> ScrapeQuery -> IO Scrape
scrapeUDP tracker scr = do
  freshConnection tracker
  resp <- transaction tracker (Scrape scr)
  case resp of
    Scraped info -> return $ M.fromList $ L.zip scr info
    _            -> fail "scrape: response type mismatch"

{-----------------------------------------------------------------------
  Retransmission
-----------------------------------------------------------------------}

sec :: Int
sec = 1000000

minTimeout :: Int
minTimeout = 15 * sec

maxTimeout :: Int
maxTimeout = 15 * 2 ^ (8 :: Int) * sec

retransmission :: IO a -> IO a
retransmission action = go minTimeout
  where
    go curTimeout
      | maxTimeout < curTimeout = throwIO $ userError "tracker down"
      |       otherwise         = do
        r <- timeout curTimeout action
        maybe (go (2 * curTimeout)) return r

{----------------------------------------------------------------------}

instance Tracker UDPTracker where
  connect    = initialTracker
  announce t = retransmission . announceUDP t
  scrape   t = retransmission . scrapeUDP   t
