{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
module Network.BitTorrent.Exchange.Session
       ( Session
       , LogFun
       , newSession
       , closeSession

       , Network.BitTorrent.Exchange.Session.insert
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception hiding (Handler)
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.List as CL (iterM)
import Data.Function
import Data.IORef
import Data.List as L
import Data.Maybe
import Data.Map as M
import Data.Monoid
import Data.Ord
import Data.Set as S
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
import qualified Data.Torrent.Piece as Torrent (Piece (Piece))
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Assembler
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

data ConnectionEntry = ConnectionEntry
  { initiatedBy :: !ChannelSide
  , connection  :: !(Connection Session)
  }

data Session = Session
  { tpeerId      :: PeerId
  , infohash     :: InfoHash
  , metadata     :: MVar Metadata.Status
  , storage      :: Storage
  , status       :: MVar SessionStatus
  , unchoked     :: [PeerAddr IP]
  , connections  :: MVar (Map (PeerAddr IP) ConnectionEntry)
  , broadcast    :: Chan Message
  , logger       :: LogFun
  , infodict     :: MVar (Cached InfoDict)
  }

-- | Logger function.
type LogFun = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

newSession :: LogFun
           -> PeerAddr (Maybe IP) -- ^ /external/ address of this peer;
           -> FilePath            -- ^ root directory for content files;
           -> InfoDict            -- ^ torrent info dictionary;
           -> IO Session          -- ^
newSession logFun addr rootPath dict = do
  connVar     <- newMVar M.empty
  store       <- openInfoDict ReadWriteEx rootPath dict
  statusVar   <- newMVar $ sessionStatus (BF.haveNone (totalPieces store))
                                         (piPieceLength (idPieceInfo dict))
  chan        <- newChan
  return Session
    { tpeerId     = fromMaybe (error "newSession: impossible") (peerId addr)
    , infohash    = idInfoHash dict
    , status      = statusVar
    , storage     = store
    , unchoked    = []
    , connections = connVar
    , broadcast   = chan
    , logger      = logFun
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
--  Connections
-----------------------------------------------------------------------}
-- TODO unmap storage on zero connections

insert :: PeerAddr IP
       -> {- Maybe Socket
       -> -} Session -> IO ()
insert addr ses @ Session {..} = void $ forkIO (action `finally` cleanup)
  where
    cleanup = do
      runStatusUpdates status (SS.resetPending addr)
      -- TODO Metata.resetPending addr

    action = do
      let caps  = def
      let ecaps = def
      let hs = Handshake def caps infohash tpeerId
      chan <- dupChan broadcast
      connectWire ses hs addr ecaps chan $ do
        conn <- ask
--      liftIO $ modifyMVar_ connections $ pure . M.insert addr conn
        lift $ resizeBitfield (totalPieces storage)
        logEvent "Connection established"
        iterM logMessage =$= exchange =$= iterM logMessage
--      liftIO $ modifyMVar_ connections $ pure . M.delete addr

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
  liftIO $ runUpdates metadata m

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
  addr <- asks   connRemoteAddr
  mpix <- lift $ withMetadataUpdates (Metadata.scheduleBlock addr)
  case mpix of
    Nothing  -> undefined
    Just pix -> sendMessage (MetadataRequest pix)

handleMetadata :: Handler ExtendedMetadata
handleMetadata (MetadataRequest pix) =
    lift (tryReadMetadataBlock pix) >>= sendMessage . mkResponse
  where
    mkResponse  Nothing              = MetadataReject pix
    mkResponse (Just (piece, total)) = MetadataData   piece total

handleMetadata (MetadataData   {..}) = do
  addr <- asks connRemoteAddr
  ih   <- asks connTopic
  lift $ withMetadataUpdates (Metadata.pushBlock addr piece ih)
  tryRequestMetadataBlock

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
