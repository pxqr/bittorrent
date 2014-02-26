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
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeFamilies               #-}
module Network.BitTorrent.Tracker.RPC.UDP
       ( -- * Manager
         Options (..)
       , Manager
       , newManager
       , closeManager
       , withManager

         -- * RPC
       , RpcException (..)
       , announce
       , scrape
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
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
import Data.Time.Clock.POSIX
import Data.Traversable
import Data.Typeable
import Data.Word
import Text.Read (readMaybe)
import Network.Socket hiding (Connected, connect, listen)
import Network.Socket.ByteString as BS
import Network.URI
import System.Entropy
import System.Timeout
import Numeric

import Network.BitTorrent.Tracker.Message

{-----------------------------------------------------------------------
--  Options
-----------------------------------------------------------------------}

sec :: Int
sec = 1000000

defMinTimeout :: Int
defMinTimeout = 15 * sec

defMaxTimeout :: Int
defMaxTimeout = 15 * 2 ^ (8 :: Int) * sec

defMultiplier :: Int
defMultiplier = 2

-- announce request packet
defMaxPacketSize :: Int
defMaxPacketSize = 98

data Options = Options
  { optMaxPacketSize :: {-# UNPACK #-} !Int

    -- | in seconds.
  , optMinTimeout    :: {-# UNPACK #-} !Int

    -- | in seconds.
  , optMaxTimeout    :: {-# UNPACK #-} !Int

  , optMultiplier    :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)

instance Default Options where
  def = Options
    { optMaxPacketSize = defMaxPacketSize
    , optMinTimeout    = defMinTimeout
    , optMaxTimeout    = defMaxTimeout
    , optMultiplier    = defMultiplier
    }

{-----------------------------------------------------------------------
--  Manager state
-----------------------------------------------------------------------}

type ConnectionCache     = Map SockAddr Connection

type PendingResponse     = MVar (Either RpcException Response)
type PendingTransactions = Map TransactionId PendingResponse
type PendingQueries      = Map SockAddr      PendingTransactions

data Manager = Manager
  { options         :: !Options
  , sock            :: !Socket
--  , dnsCache        :: !(IORef (Map URI SockAddr))
  , connectionCache :: !(IORef ConnectionCache)
  , pendingResps    :: !(MVar  PendingQueries)
  , listenerThread  :: !(MVar ThreadId)
  }

initManager :: Options -> IO Manager
initManager opts = Manager opts
  <$> socket AF_INET Datagram defaultProtocol
  <*> newIORef M.empty
  <*> newMVar  M.empty
  <*> newEmptyMVar

unblockAll :: PendingQueries -> IO ()
unblockAll m = traverse (traverse unblock) m >> return ()
  where
    unblock ares = putMVar ares (Left ManagerClosed)

resetState :: Manager -> IO ()
resetState Manager {..} = do
    writeIORef          connectionCache err
    m    <- swapMVar    pendingResps    err
    unblockAll m
    mtid <- tryTakeMVar listenerThread
    case mtid of
      Nothing -> return () -- thread killed by 'closeManager'
      Just _  -> return () -- thread killed by exception from 'listen'
    return ()
  where
    err = error "UDP tracker manager closed"

newManager :: Options -> IO Manager
newManager opts = do
  mgr <- initManager opts
  tid <- forkIO (listen mgr `finally` resetState mgr)
  putMVar (listenerThread mgr) tid
  return mgr

closeManager :: Manager -> IO ()
closeManager Manager {..} = do
  close sock
  mtid <- tryTakeMVar listenerThread
  case mtid of
    Nothing  -> return ()
    Just tid -> killThread tid

withManager :: Options -> (Manager -> IO a) -> IO a
withManager opts = bracket (newManager opts) closeManager

{-----------------------------------------------------------------------
--  Exceptions
-----------------------------------------------------------------------}

data RpcException
    -- | Unable to lookup hostname;
  = HostUnknown

    -- | Unable to lookup hostname;
  | HostLookupFailed

    -- | Tracker exists but not responding for specific number of seconds.
  | TimeoutExpired Int

    -- | Tracker responded with unexpected message type.
  | UnexpectedResponse
    { expectedMsg :: String
    , actualMsg   :: String
    }

    -- | RPC succeed, but tracker responded with error code.
  | QueryFailed Text

    -- | RPC manager closed while waiting for response.
  | ManagerClosed
    deriving (Show, Typeable)

instance Exception RpcException

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
    _                 -> throwIO HostLookupFailed
resolveURI _       = throwIO HostUnknown

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

initialConnectionId :: ConnectionId
initialConnectionId =  ConnectionId 0x41727101980

-- | Transaction Id is used within a UDP RPC.
newtype TransactionId = TransactionId Word32
  deriving (Eq, Ord, Enum, Bounded, Serialize)

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

responseName :: Response -> String
responseName (Connected _) = "connected"
responseName (Announced _) = "announced"
responseName (Scraped   _) = "scraped"
responseName (Failed    _) = "failed"

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

data Connection = Connection
  { connectionId        :: ConnectionId
  , connectionTimestamp :: UTCTime
  } deriving Show

-- placeholder for the first 'connect'
initialConnection :: Connection
initialConnection = Connection initialConnectionId (posixSecondsToUTCTime 0)

establishedConnection :: ConnectionId -> IO Connection
establishedConnection cid = Connection cid <$> getCurrentTime

isExpired :: Connection -> IO Bool
isExpired Connection {..} = do
  currentTime <- getCurrentTime
  let timeDiff = diffUTCTime currentTime connectionTimestamp
  return $ timeDiff > connectionLifetime

{-----------------------------------------------------------------------
--  Transactions
-----------------------------------------------------------------------}

-- | Sometimes 'genTransactionId' may return already used transaction
-- id. We use a good entropy source but the issue /still/ (with very
-- small probabality) may happen. If the collision happen then this
-- function tries to find nearest unused slot, otherwise pending
-- transactions table is full.
firstUnused :: SockAddr -> TransactionId -> PendingQueries -> TransactionId
firstUnused addr rid m = do
  case M.splitLookup rid <$> M.lookup addr m of
    Nothing                -> rid
    Just (_ , Nothing, _ ) -> rid
    Just (lt, Just _ , gt) ->
      case backwardHole (keys lt) rid <|> forwardHole rid (keys gt) of
        Nothing  -> error "firstUnused: table is full" -- impossible
        Just tid -> tid
  where
    forwardHole a []
      | a == maxBound = Nothing
      |   otherwise   = Just (succ a)
    forwardHole a (b : xs)
      | succ a == b   = forwardHole b xs
      |   otherwise   = Just (succ a)

    backwardHole [] a
      | a == minBound = Nothing
      |   otherwise   = Just (pred a)
    backwardHole (b : xs) a
      | b == pred a   = backwardHole xs b
      |   otherwise   = Just (pred a)

register :: SockAddr -> TransactionId -> PendingResponse
         -> PendingQueries -> PendingQueries
register addr tid ares = M.alter insertId addr
  where
    insertId Nothing  = Just (M.singleton tid ares)
    insertId (Just m) = Just (M.insert tid ares m)

unregister :: SockAddr -> TransactionId
           -> PendingQueries -> PendingQueries
unregister addr tid = M.update deleteId addr
  where
    deleteId m
      | M.null m' = Nothing
      | otherwise = Just m'
      where
        m' = M.delete tid m

-- | Generate a new unused transaction id and register as pending.
allocTransaction :: Manager -> SockAddr -> PendingResponse -> IO TransactionId
allocTransaction Manager {..} addr ares =
  modifyMVar pendingResps $ \ m -> do
    rndId <- genTransactionId
    let tid = firstUnused addr rndId m
    return (register addr tid ares m, tid)

-- | Wake up blocked thread and return response back.
commitTransaction :: Manager -> SockAddr -> TransactionId -> Response -> IO ()
commitTransaction Manager {..} addr tid resp =
  modifyMVarMasked_ pendingResps $ \ m -> do
    case M.lookup tid =<< M.lookup addr m of
      Nothing   -> return m -- tracker responded after 'cancelTransaction' fired
      Just ares -> do
        putMVar ares (Right resp)
        return $ unregister addr tid m

-- | Abort transaction forcefully.
cancelTransaction :: Manager -> SockAddr -> TransactionId -> IO ()
cancelTransaction Manager {..} addr tid =
  modifyMVarMasked_ pendingResps $ \m ->
    return $ unregister addr tid m

-- | Handle responses from trackers.
listen :: Manager -> IO ()
listen mgr @ Manager {..} = do
  forever $ do
    (bs, addr) <- BS.recvFrom sock (optMaxPacketSize options)
    case decode bs of
      Left  _                   -> return () -- parser failed, ignoring
      Right (TransactionR {..}) -> commitTransaction mgr addr transIdR response

-- | Perform RPC transaction. If the action interrupted transaction
-- will be aborted.
transaction :: Manager -> SockAddr -> Connection -> Request -> IO Response
transaction mgr @ Manager {..} addr conn request = do
    ares <- newEmptyMVar
    tid  <- allocTransaction mgr addr ares
    performTransaction tid ares
      `onException` cancelTransaction mgr addr tid
  where
    performTransaction tid ares = do
      let trans = TransactionQ (connectionId conn) tid request
      BS.sendAllTo sock (encode trans) addr
      takeMVar ares >>= either throwIO return

{-----------------------------------------------------------------------
--  Connection cache
-----------------------------------------------------------------------}

connect :: Manager -> SockAddr -> Connection -> IO ConnectionId
connect m addr conn = do
  resp <- transaction m addr conn Connect
  case resp of
    Connected cid -> return cid
    Failed    msg -> throwIO $ QueryFailed msg
    _ -> throwIO $ UnexpectedResponse "connected" (responseName resp)

newConnection :: Manager -> SockAddr -> IO Connection
newConnection m addr = do
  connId  <- connect m addr initialConnection
  establishedConnection connId

refreshConnection :: Manager -> SockAddr -> Connection -> IO Connection
refreshConnection mgr addr conn = do
  expired <- isExpired conn
  if expired
    then do
      connId <- connect mgr addr conn
      establishedConnection connId
    else do
      return conn

withCache :: Manager -> SockAddr
          -> (Maybe Connection -> IO Connection) -> IO Connection
withCache mgr addr action = do
  cache <- readIORef (connectionCache mgr)
  conn  <- action (M.lookup addr cache)
  writeIORef (connectionCache mgr) (M.insert addr conn cache)
  return conn

getConnection :: Manager -> SockAddr -> IO Connection
getConnection mgr addr = withCache mgr addr $
  maybe (newConnection mgr addr) (refreshConnection mgr addr)

{-----------------------------------------------------------------------
--  RPC
-----------------------------------------------------------------------}

retransmission :: Options -> IO a -> IO a
retransmission Options {..} action = go optMinTimeout
  where
    go curTimeout
      | curTimeout > optMaxTimeout = throwIO $ TimeoutExpired curTimeout
      |         otherwise          = do
        r <- timeout curTimeout action
        maybe (go (optMultiplier * curTimeout)) return r

queryTracker :: Manager -> URI -> Request -> IO Response
queryTracker mgr uri req = do
  addr <- getTrackerAddr mgr uri
  retransmission (options mgr) $ do
    conn <- getConnection  mgr addr
    transaction mgr addr conn req

-- | This function can throw 'RpcException'.
announce :: Manager -> URI -> AnnounceQuery -> IO AnnounceInfo
announce mgr uri q = do
  resp <- queryTracker mgr uri (Announce q)
  case resp of
    Announced info -> return info
    _ -> throwIO $ UnexpectedResponse "announce" (responseName resp)

-- | This function can throw 'RpcException'.
scrape :: Manager -> URI -> ScrapeQuery -> IO ScrapeInfo
scrape mgr uri ihs = do
  resp <- queryTracker mgr uri (Scrape ihs)
  case resp of
    Scraped info -> return $ L.zip ihs info
    _ -> throwIO $ UnexpectedResponse "scrape" (responseName resp)
