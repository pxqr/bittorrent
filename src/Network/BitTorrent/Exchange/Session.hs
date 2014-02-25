{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
module Network.BitTorrent.Exchange.Session
       ( Session
       , LogFun
       , newSession
       , closeSession

         -- * Connections
       , Network.BitTorrent.Exchange.Session.insert
       , Network.BitTorrent.Exchange.Session.attach
       , Network.BitTorrent.Exchange.Session.delete
       , Network.BitTorrent.Exchange.Session.deleteAll
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (Handler)
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.List as CL (iterM)
import Data.Maybe
import Data.Map as M
import Data.Monoid
import Data.Set  as S
import Data.Text as T
import Data.Typeable
import Text.PrettyPrint hiding ((<>))
import Text.PrettyPrint.Class
import System.Log.FastLogger (LogStr, ToLogStr (..))

import Data.BEncode as BE
import Data.Torrent (InfoDict (..))
import Data.Torrent.Bitfield as BF
import Data.Torrent.InfoHash
import Data.Torrent.Piece
import qualified Data.Torrent.Piece as Torrent (Piece ())
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Block   as Block
import Network.BitTorrent.Exchange.Message as Message
import Network.BitTorrent.Exchange.Session.Metadata as Metadata
import Network.BitTorrent.Exchange.Session.Status   as SS
import Network.BitTorrent.Exchange.Wire
import Network.BitTorrent.Exchange.Wire.Status
import System.Torrent.Storage

{-----------------------------------------------------------------------
--  Exceptions
-----------------------------------------------------------------------}

data ExchangeError
  = InvalidRequest BlockIx StorageFailure
  | CorruptedPiece PieceIx
    deriving (Show, Typeable)

instance Exception ExchangeError

packException :: Exception e => (e -> ExchangeError) -> IO a -> IO a
packException f m = try m >>= either (throwIO . f) return

{-----------------------------------------------------------------------
--  Session
-----------------------------------------------------------------------}

data Cached a = Cached
  { cachedValue :: !a
  , cachedData  :: BL.ByteString -- keep lazy
  }

cache :: BEncode a => a -> Cached a
cache s = Cached s (BE.encode s)

data Session = Session
  { sessionPeerId          :: !(PeerId)
  , sessionTopic           :: !(InfoHash)

  , metadata               :: !(MVar Metadata.Status)
  , infodict               :: !(MVar (Cached InfoDict))

  , status                 :: !(MVar SessionStatus)
  , storage                :: !(Storage)

  , broadcast              :: !(Chan Message)

  , unchoked               ::  [PeerAddr IP]
  , connectionsPrefs       :: !ConnectionPrefs
  , connectionsPending     :: !(TVar (Set (PeerAddr IP)))
  , connectionsEstablished :: !(TVar (Map (PeerAddr IP) (Connection Session)))

  , logger                 :: !(LogFun)
  }

instance Ord IP

-- | Logger function.
type LogFun = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

newSession :: LogFun
           -> PeerAddr (Maybe IP) -- ^ /external/ address of this peer;
           -> FilePath            -- ^ root directory for content files;
           -> InfoDict            -- ^ torrent info dictionary;
           -> IO Session          -- ^
newSession logFun addr rootPath dict = do
  pid         <- maybe genPeerId return (peerId addr)
  pconnVar    <- newTVarIO S.empty
  econnVar    <- newTVarIO M.empty
  store       <- openInfoDict ReadWriteEx rootPath dict
  statusVar   <- newMVar $ sessionStatus (BF.haveNone (totalPieces store))
                                         (piPieceLength (idPieceInfo dict))
  chan        <- newChan
  return Session
    { sessionPeerId          = pid
    , sessionTopic           = idInfoHash dict
    , status                 = statusVar
    , storage                = store
    , unchoked               = []
    , connectionsPrefs       = def
    , connectionsPending     = pconnVar
    , connectionsEstablished = econnVar
    , broadcast              = chan
    , logger                 = logFun
    , metadata               = undefined
    , infodict               = undefined
    }

closeSession :: Session -> IO ()
closeSession ses = do
  deleteAll ses
  undefined

{-----------------------------------------------------------------------
--  Logging
-----------------------------------------------------------------------}

instance MonadLogger (Connected Session) where
  monadLoggerLog loc src lvl msg = do
    conn <- ask
    ses  <- asks connSession
    addr <- asks connRemoteAddr
    let addrSrc = src <> " @ " <> T.pack (render (pretty addr))
    liftIO $ logger ses loc addrSrc lvl (toLogStr msg)

logMessage :: MonadLogger m => Message -> m ()
logMessage msg = logDebugN $ T.pack (render (pretty msg))

logEvent   :: MonadLogger m => Text    -> m ()
logEvent       = logInfoN

{-----------------------------------------------------------------------
--  Connection slots
-----------------------------------------------------------------------}
--- pending -> established -> closed
---    |                        /|\
---    \-------------------------|

pendingConnection :: PeerAddr IP -> Session -> IO Bool
pendingConnection addr Session {..} = atomically $ do
  pSet <- readTVar connectionsPending
  eSet <- readTVar connectionsEstablished
  if (addr `S.member` pSet) || (addr `M.member` eSet)
    then return False
    else do
      modifyTVar' connectionsPending (S.insert addr)
      return True

establishedConnection :: Connected Session ()
establishedConnection = undefined --atomically $ do
--  pSet <- readTVar pendingConnections
--  eSet <- readTVar
  undefined

finishedConnection :: Connected Session ()
finishedConnection = return ()

-- | There are no state for this connection, remove it.
closedConnection :: PeerAddr IP -> Session -> IO ()
closedConnection addr Session {..} = atomically $ do
  modifyTVar connectionsPending     $ S.delete addr
  modifyTVar connectionsEstablished $ M.delete addr

{-----------------------------------------------------------------------
--  Connections
-----------------------------------------------------------------------}
-- TODO unmap storage on zero connections

mainWire :: Wire Session ()
mainWire = do
  lift establishedConnection
  Session {..} <- asks connSession
  lift $ resizeBitfield (totalPieces storage)
  logEvent "Connection established"
  iterM logMessage =$= exchange =$= iterM logMessage
  lift finishedConnection

getConnectionConfig :: Session -> IO (ConnectionConfig Session)
getConnectionConfig s @ Session {..} = do
  chan <- dupChan broadcast
  let sessionLink = SessionLink {
      linkTopic        = sessionTopic
    , linkPeerId       = sessionPeerId
    , linkMetadataSize = Nothing
    , linkOutputChan   = Just chan
    , linkSession      = s
    }
  return ConnectionConfig
    { cfgPrefs   = connectionsPrefs
    , cfgSession = sessionLink
    , cfgWire    = mainWire
    }

insert :: PeerAddr IP -> Session -> IO ()
insert addr ses @ Session {..} = void $ forkIO (action `finally` cleanup)
  where
    action = do
      pendingConnection addr ses
      cfg <- getConnectionConfig ses
      connectWire addr cfg

    cleanup = do
      runStatusUpdates status (SS.resetPending addr)
      -- TODO Metata.resetPending addr
      closedConnection addr ses

-- TODO closePending on error
attach :: PendingConnection -> Session -> IO ()
attach = undefined

delete :: PeerAddr IP -> Session -> IO ()
delete = undefined

deleteAll :: Session -> IO ()
deleteAll = undefined

{-----------------------------------------------------------------------
--  Helpers
-----------------------------------------------------------------------}

waitMVar :: MVar a -> IO ()
waitMVar m = withMVar m (const (return ()))

-- This function appear in new GHC "out of box". (moreover it is atomic)
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar m = do
  ma <- tryTakeMVar m
  maybe (return ()) (putMVar m) ma
  return ma

withStatusUpdates :: StatusUpdates a -> Wire Session a
withStatusUpdates m = do
  Session {..} <- asks connSession
  liftIO $ runStatusUpdates status m

withMetadataUpdates :: Updates a -> Connected Session a
withMetadataUpdates m = do
  Session {..} <- asks connSession
  addr         <- asks connRemoteAddr
  liftIO $ runUpdates metadata addr m

getThisBitfield :: Wire Session Bitfield
getThisBitfield = do
  ses <- asks connSession
  liftIO $ SS.getBitfield (status ses)

readBlock :: BlockIx -> Storage -> IO (Block BL.ByteString)
readBlock bix @ BlockIx {..} s = do
  p <- packException (InvalidRequest bix) $ do readPiece ixPiece s
  let chunk = BL.take (fromIntegral ixLength) $
              BL.drop (fromIntegral ixOffset) (pieceData p)
  if BL.length chunk == fromIntegral ixLength
    then return  $ Block ixPiece ixOffset chunk
    else throwIO $ InvalidRequest bix (InvalidSize ixLength)

-- |
tryReadMetadataBlock :: PieceIx
   -> Connected Session (Maybe (Torrent.Piece BS.ByteString, Int))
tryReadMetadataBlock pix = do
  Session {..} <- asks connSession
  mcached      <- liftIO (tryReadMVar infodict)
  case mcached of
    Nothing            -> undefined
    Just (Cached {..}) -> undefined

sendBroadcast :: PeerMessage msg => msg -> Wire Session ()
sendBroadcast msg = do
  Session {..} <- asks connSession
  ecaps <- use connExtCaps
  liftIO $ writeChan broadcast (envelop ecaps msg)

{-----------------------------------------------------------------------
--  Triggers
-----------------------------------------------------------------------}

fillRequestQueue :: Wire Session ()
fillRequestQueue = do
  maxN <- lift getMaxQueueLength
  rbf  <- use connBitfield
  addr <- asks connRemoteAddr
  blks <- withStatusUpdates $ do
    n <- getRequestQueueLength addr
    scheduleBlocks addr rbf (maxN - n)
  mapM_ (sendMessage . Request) blks

tryFillRequestQueue :: Wire Session ()
tryFillRequestQueue = do
  allowed <- canDownload <$> use connStatus
  when allowed $ do
    fillRequestQueue

interesting :: Wire Session ()
interesting = do
  addr <- asks connRemoteAddr
  sendMessage (Interested True)
  sendMessage (Choking    False)
  tryFillRequestQueue

{-----------------------------------------------------------------------
--  Incoming message handling
-----------------------------------------------------------------------}

type Handler msg = msg -> Wire Session ()

handleStatus :: Handler StatusUpdate
handleStatus s = do
  connStatus %= over remoteStatus (updateStatus s)
  case s of
    Interested _     -> return ()
    Choking    True  -> do
      addr <- asks connRemoteAddr
      withStatusUpdates (SS.resetPending addr)
    Choking    False -> tryFillRequestQueue

handleAvailable :: Handler Available
handleAvailable msg = do
  connBitfield %= case msg of
    Have     ix -> BF.insert ix
    Bitfield bf -> const     bf

  thisBf <- getThisBitfield
  case msg of
    Have     ix
      | ix `BF.member`     thisBf -> return ()
      |     otherwise             -> interesting
    Bitfield bf
      | bf `BF.isSubsetOf` thisBf -> return ()
      |     otherwise             -> interesting

handleTransfer :: Handler Transfer
handleTransfer (Request bix) = do
  Session {..} <- asks connSession
  bitfield <- getThisBitfield
  upload   <- canUpload <$> use connStatus
  when (upload && ixPiece bix `BF.member` bitfield) $ do
    blk <- liftIO $ readBlock bix storage
    sendMessage (Message.Piece blk)

handleTransfer (Message.Piece   blk) = do
  Session {..} <- asks connSession
  isSuccess <- withStatusUpdates (SS.pushBlock blk storage)
  case isSuccess of
    Nothing -> liftIO $ throwIO $ userError "block is not requested"
    Just isCompleted -> do
      when isCompleted $ do
        sendBroadcast (Have (blkPiece blk))
--        maybe send not interested
      tryFillRequestQueue

handleTransfer (Cancel  bix) = filterQueue (not . (transferResponse bix))
  where
    transferResponse bix (Transfer (Message.Piece blk)) = blockIx blk == bix
    transferResponse _    _                             = False

{-----------------------------------------------------------------------
--  Metadata exchange
-----------------------------------------------------------------------}
-- TODO introduce new metadata exchange specific exceptions

tryRequestMetadataBlock :: Wire Session ()
tryRequestMetadataBlock = do
  mpix <- lift $ withMetadataUpdates Metadata.scheduleBlock
  case mpix of
    Nothing  -> undefined
    Just pix -> sendMessage (MetadataRequest pix)

metadataCompleted :: InfoDict -> Wire Session ()
metadataCompleted dict = do
  Session {..} <- asks connSession
  liftIO $ putMVar infodict (cache dict)

handleMetadata :: Handler ExtendedMetadata
handleMetadata (MetadataRequest pix) =
    lift (tryReadMetadataBlock pix) >>= sendMessage . mkResponse
  where
    mkResponse  Nothing              = MetadataReject pix
    mkResponse (Just (piece, total)) = MetadataData   piece total

handleMetadata (MetadataData   {..}) = do
  ih    <- asks connTopic
  mdict <- lift $ withMetadataUpdates (Metadata.pushBlock piece ih)
  case mdict of
    Nothing   -> tryRequestMetadataBlock -- not completed, need all blocks
    Just dict -> metadataCompleted dict  -- complete, wake up payload fetch

handleMetadata (MetadataReject  pix) = do
  lift $ withMetadataUpdates (Metadata.cancelPending pix)

handleMetadata (MetadataUnknown _  ) = do
  logInfoN "Unknown metadata message"

waitForMetadata :: Wire Session ()
waitForMetadata = do
  Session {..} <- asks connSession
  needFetch    <- liftIO (isEmptyMVar infodict)
  when needFetch $ do
    canFetch   <- allowed ExtMetadata <$> use connExtCaps
    if canFetch
      then tryRequestMetadataBlock
      else liftIO (waitMVar infodict)

{-----------------------------------------------------------------------
--  Event loop
-----------------------------------------------------------------------}

acceptRehandshake :: ExtendedHandshake -> Wire s ()
acceptRehandshake ehs = undefined

handleExtended :: Handler ExtendedMessage
handleExtended (EHandshake     ehs) = acceptRehandshake ehs
handleExtended (EMetadata  _   msg) = handleMetadata msg
handleExtended (EUnknown   _   _  ) = logWarnN "Unknown extension message"

handleMessage :: Handler Message
handleMessage KeepAlive       = return ()
handleMessage (Status s)      = handleStatus s
handleMessage (Available msg) = handleAvailable msg
handleMessage (Transfer  msg) = handleTransfer msg
handleMessage (Port      n)   = undefined
handleMessage (Fast      _)   = undefined
handleMessage (Extended  msg) = handleExtended msg

exchange :: Wire Session ()
exchange = do
  waitForMetadata
  bf <- getThisBitfield
  sendMessage (Bitfield bf)
  awaitForever handleMessage

data Event = NewMessage (PeerAddr IP) Message
           | Timeout -- for scheduling

type Exchange a = Wire Session a

awaitEvent :: Exchange Event
awaitEvent = undefined

yieldEvent :: Exchange Event
yieldEvent = undefined
