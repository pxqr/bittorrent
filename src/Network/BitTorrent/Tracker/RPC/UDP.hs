-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
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
module Network.BitTorrent.Tracker.RPC.UDP
       ( -- * Manager
         Options (..)
       , Manager
       , newManager
       , closeManager
       , withManager

         -- * RPC
       , announce
       , scrape
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Default
import Data.IORef
import Data.List as L
import Data.Map  as M
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.Text as T
import Data.Text.Encoding
import Data.Time
import Data.Word
import Text.Read (readMaybe)
import Network.Socket hiding (Connected, connect)
import Network.Socket.ByteString as BS
import Network.URI
import System.Entropy
import System.Timeout
import Numeric

import Network.BitTorrent.Tracker.Message

{-----------------------------------------------------------------------
--  Manager
-----------------------------------------------------------------------}

sec :: Int
sec = 1000000

defMinTimeout :: Int
defMinTimeout = 15 * sec

defMaxTimeout :: Int
defMaxTimeout = 15 * 2 ^ (8 :: Int) * sec

data Options = Options
  { optMinTimeout :: {-# UNPACK #-} !Int
  , optMaxTimeout :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)

instance Default Options where
  def = Options
    { optMinTimeout = defMinTimeout
    , optMaxTimeout = defMaxTimeout
    }

data Manager = Manager
  { options         :: !Options
  , sock            :: !Socket
--  , dnsCache        :: !(IORef (Map URI SockAddr))
  , connectionCache :: !(IORef (Map SockAddr Connection))
--  , pendingResps    :: !(IORef (Map Connection [MessageId]))
  }

newManager :: Options -> IO Manager
newManager opts = Manager opts
  <$> socket AF_INET Datagram defaultProtocol
  <*> newIORef M.empty

closeManager :: Manager -> IO ()
closeManager Manager {..} = close sock

withManager :: Options -> (Manager -> IO a) -> IO a
withManager opts = bracket (newManager opts) closeManager

{-----------------------------------------------------------------------
--  Host Addr resolution
-----------------------------------------------------------------------}

setPort :: PortNumber -> SockAddr -> SockAddr
setPort p (SockAddrInet  _ h)     = SockAddrInet  p h
setPort p (SockAddrInet6 _ f h s) = SockAddrInet6 p f h s
setPort _  addr = addr

resolveURI :: URI -> IO SockAddr
resolveURI URI { uriAuthority = Just (URIAuth {..}) } = do
  infos <- getAddrInfo Nothing (Just uriRegName) Nothing
  let port = fromMaybe 0 (readMaybe (L.drop 1 uriPort) :: Maybe Int)
  case infos of
    AddrInfo {..} : _ -> return $ setPort (fromIntegral port) addrAddress
    _                 -> fail "getTrackerAddr: unable to lookup host addr"
resolveURI _       = fail "getTrackerAddr: hostname unknown"

-- TODO caching?
getTrackerAddr :: Manager -> URI -> IO SockAddr
getTrackerAddr _ = resolveURI

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
              | Scraped   [ScrapeEntry]
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

call :: Manager -> SockAddr -> ByteString -> IO ByteString
call Manager {..} addr arg = do
  BS.sendAllTo sock arg addr
  (res, addr') <- BS.recvFrom sock maxPacketSize
  unless (addr' == addr) $ do
    throwIO $ userError "address mismatch"
  return res

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

transaction :: Manager -> UDPTracker -> Request -> IO Response
transaction m tracker @ UDPTracker {..} request = do
  cid <- getConnectionId tracker
  tid <- genTransactionId
  let trans = TransactionQ cid tid request

  addr <- getTrackerAddr m trackerURI
  res  <- call m addr (encode trans)
  case decode res of
    Right (TransactionR {..})
      | tid == transIdR -> return response
      |   otherwise     -> throwIO $ userError "transaction id mismatch"
    Left msg            -> throwIO $ userError msg

connectUDP :: Manager -> UDPTracker -> IO ConnectionId
connectUDP m tracker = do
  resp <- transaction m tracker Connect
  case resp of
    Connected cid -> return cid
    Failed    msg -> throwIO $ userError $ T.unpack msg
    _             -> throwIO $ userError "connect: response type mismatch"

connect :: Manager -> URI -> IO UDPTracker
connect m uri = do
  tracker <- UDPTracker uri <$> (newIORef =<< initialConnection)
  connId  <- connectUDP m tracker
  updateConnection connId tracker
  return tracker

freshConnection :: Manager -> UDPTracker -> IO ()
freshConnection m tracker @ UDPTracker {..} = do
  conn    <- readIORef trackerConnection
  expired <- isExpired conn
  when expired $ do
    connId <- connectUDP m tracker
    updateConnection connId tracker

getConnection :: Manager -> URI -> IO Connection
getConnection _ = undefined

announce :: Manager -> AnnounceQuery -> UDPTracker -> IO AnnounceInfo
announce m ann tracker = do
  freshConnection m tracker
  resp <- transaction m tracker (Announce ann)
  case resp of
    Announced info -> return info
    _              -> fail "announce: response type mismatch"

scrape :: Manager -> ScrapeQuery -> UDPTracker -> IO ScrapeInfo
scrape m ihs tracker = do
  freshConnection m tracker
  resp <- transaction m tracker (Scrape ihs)
  case resp of
    Scraped info -> return $ L.zip ihs info
    _            -> fail "scrape: response type mismatch"

retransmission :: Options -> IO a -> IO a
retransmission Options {..} action = go optMinTimeout
  where
    go curTimeout
      | curTimeout > optMaxTimeout = throwIO $ userError "tracker down"
      |         otherwise          = do
        r <- timeout curTimeout action
        maybe (go (2 * curTimeout)) return r
