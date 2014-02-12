{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.BitTorrent.Exchange.Session
       ( Session
       , newSession
       , closeSession

       , Network.BitTorrent.Exchange.Session.insert
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Map as M
import Data.Ord
import Data.Typeable
import Text.PrettyPrint

import Data.Torrent (InfoDict (..))
import Data.Torrent.Bitfield as BF
import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Assembler
import Network.BitTorrent.Exchange.Block
import Network.BitTorrent.Exchange.Message
import Network.BitTorrent.Exchange.Status
import Network.BitTorrent.Exchange.Wire
import System.Torrent.Storage


data Session = Session
  { tpeerId     :: PeerId
  , infohash    :: InfoHash
  , bitfield    :: Bitfield
  , assembler   :: Assembler
  , storage     :: Storage
  , unchoked    :: [PeerAddr IP]
  , connections :: MVar (Map (PeerAddr IP) (Connection Session))
  , broadcast   :: Chan Message
  }

newSession :: PeerAddr (Maybe IP) -- ^ /external/ address of this peer;
           -> FilePath            -- ^ root directory for content files;
           -> InfoDict            -- ^ torrent info dictionary;
           -> IO Session          -- ^
newSession addr rootPath dict = do
  connVar <- newMVar M.empty
  store   <- openInfoDict ReadWriteEx rootPath dict
  chan    <- newChan
  return Session
    { tpeerId     = fromMaybe (error "newSession: impossible") (peerId addr)
    , infohash    = idInfoHash dict
    , bitfield    = BF.haveNone (totalPieces store)
    , assembler   = error "newSession"
    , storage     = store
    , unchoked    = []
    , connections = connVar
    , broadcast   = chan
    }

closeSession :: Session -> IO ()
closeSession = undefined

insert :: PeerAddr IP
       -> {- Maybe Socket
       -> -} Session -> IO ()
insert addr ses @ Session {..} = do
  forkIO $ do
    let caps  = def
    let ecaps = def
    let hs = Handshake def caps infohash tpeerId
    chan <- dupChan broadcast
    connectWire ses hs addr ecaps chan $ do
      conn <- getConnection
--      liftIO $ modifyMVar_ connections $ pure . M.insert addr conn
      exchange
--      liftIO $ modifyMVar_ connections $ pure . M.delete addr
  return ()

delete :: PeerAddr IP -> Session -> IO ()
delete = undefined

deleteAll :: Session -> IO ()
deleteAll = undefined

{-----------------------------------------------------------------------
--  Query
-----------------------------------------------------------------------}

getThisBitfield :: Wire Session Bitfield
getThisBitfield = undefined

{-
data PendingSet = PendingSet (Map (PeerAddr IP) [BlockIx])

empty :: PendingSet
empty = undefined

member :: PeerAddr IP -> BlockIx -> PendingSet -> Bool
member addr bix = undefined

insert :: PeerAddr IP -> BlockIx -> PendingSet -> PendingSet
insert addr bix = undefined
-}
{-----------------------------------------------------------------------
--  Event loop
-----------------------------------------------------------------------}
{-
data ExchangeError
  = InvalidRequest BlockIx StorageFailure
  | CorruptedPiece PieceIx

packException :: Exception e => (e -> ExchangeError) -> IO a -> IO a
packException f m = try >>= either (throwIO . f) m

readBlock :: BlockIx -> Storage -> IO (Block ByteString)
readBlock bix @ BlockIx {..} s = do
  p <- packException (InvalidRequest bix) $ do readPiece ixPiece storage
  let chunk = BS.take ixLength $ BS.drop ixOffset p
  if BS.length chunk == ixLength
    then return chunk
    else throwIO $ InvalidRequest bix (InvalidSize ixLength)
-}

handleMessage :: Message -> Wire Session ()
handleMessage KeepAlive       = return ()
handleMessage (Status s)      = undefined
handleMessage (Available msg) = do
  thisBf <- getThisBitfield
  case msg of
    Have     ix
      | ix `BF.member`     thisBf -> return ()
      |     otherwise             -> undefined
    Bitfield bf
      | bf `BF.isSubsetOf` thisBf -> return ()
      |     otherwise             -> undefined

handleMessage (Transfer  msg) = case msg of
  Request bix -> do
--    Session {..} <- getSession
--    addr <- getRemoteAddr
--    when (addr `elem` unchoked && ixPiece bix `BF.member` bitfield) $ do
--      blk <- liftIO $ readBlock bix storage
--      sendMsg (Piece blk)
    return ()

  Piece   blk -> do
{-
    Session {..} <- getSession
    when (blockIx blk `PS.member` pendingSet) $ do
      insert blk stalledSet
      sendBroadcast have
      maybe send not interested
-}
    return ()

  Cancel  bix -> filterQueue (not . (transferResponse bix))
    where
      transferResponse bix (Transfer (Piece blk)) = blockIx blk == bix
      transferResponse _    _                     = False

handleMessage (Port      n) = undefined
handleMessage (Fast      _) = undefined
handleMessage (Extended  _) = undefined

filterQueue :: (Message -> Bool) -> Wire s ()
filterQueue = undefined

exchange :: Wire Session ()
exchange = do
  e <- recvMessage
  liftIO $ print e

type Exchange = StateT Session (ReaderT (Connection Session) IO)

--runExchange :: Exchange () -> [PeerAddr] -> IO ()
--runExchange exchange peers = do
--  forM_ peers $ \ peer -> do
--    forkIO $ runReaderT (runStateT exchange session )

data Event = NewMessage (PeerAddr IP) Message
           | Timeout -- for scheduling

awaitEvent :: Exchange Event
awaitEvent = undefined

yieldEvent :: Exchange Event
yieldEvent = undefined
