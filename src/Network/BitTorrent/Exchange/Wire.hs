-- |
--   Module      :  Network.BitTorrent.Exchange.Wire
--   Copyright   :  (c) Sam Truzjan 2013
--                  (c) Daniel Gröber 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module control /integrity/ of data send and received.
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BitTorrent.Exchange.Wire
       ( -- * Wire
         Connected
       , Wire

         -- ** Exceptions
       , ChannelSide   (..)
       , ProtocolError (..)
       , WireFailure   (..)
       , peerPenalty
       , isWireFailure
       , disconnectPeer

         -- ** Stats
       , ByteStats       (..)
       , FlowStats       (..)
       , ConnectionStats (..)

         -- ** Flood detection
       , FloodDetector   (..)

         -- ** Options
       , Options         (..)

         -- ** Connection
       , Connection
       , connRemoteAddr
       , connProtocol
       , connCaps
       , connTopic
       , connRemotePeerId
       , connThisPeerId
       , connOptions
       , connSession

         -- ** Setup
       , runWire
       , connectWire
       , acceptWire
       , resizeBitfield

         -- ** Messaging
       , recvMessage
       , sendMessage
       , filterQueue
       , getAdvertisedQueueLength

         -- ** Query
       , getConnection
       , getSession
       , getStatus
       , getRemoteBitfield
       , updateConnStatus
       , updateRemoteBitfield
       , getExtCaps
       , getStats
       , getMetadata
       ) where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.List
import Data.Conduit.Network
import Data.Default
import Data.IORef
import Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Serialize as S
import Data.Typeable
import Network
import Network.Socket hiding (Connected)
import Network.Socket.ByteString as BS
import Text.PrettyPrint as PP hiding (($$), (<>))
import Text.PrettyPrint.Class
import Text.Show.Functions
import System.Log.FastLogger (ToLogStr(..))
import System.Timeout

import Data.BEncode as BE
import Data.Torrent
import Data.Torrent.Bitfield as BF
import Data.Torrent.InfoHash
import Data.Torrent.Piece
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Message as Msg
import Network.BitTorrent.Exchange.Status

-- TODO handle port message?
-- TODO handle limits?
-- TODO filter not requested PIECE messages
-- TODO metadata piece request flood protection
-- TODO piece request flood protection
-- TODO protect against flood attacks
{-----------------------------------------------------------------------
--  Exceptions
-----------------------------------------------------------------------}

-- | Used to specify initiator of 'ProtocolError'.
data ChannelSide
  = ThisPeer
  | RemotePeer
    deriving (Show, Eq, Enum, Bounded)

instance Default ChannelSide where
  def = ThisPeer

instance Pretty ChannelSide where
  pretty = PP.text . show

-- | A protocol errors occur when a peer violates protocol
-- specification.
data ProtocolError
    -- | Protocol string should be 'BitTorrent Protocol' but remote
    -- peer have sent a different string.
  = InvalidProtocol   ProtocolName

    -- | Sent and received protocol strings do not match. Can occur
    -- in 'connectWire' only.
  | UnexpectedProtocol ProtocolName

    -- | /Remote/ peer replied with invalid 'hsInfoHash' which do not
    -- match with 'hsInfoHash' /this/ peer have sent. Can occur in
    -- 'connectWire' only.
  | UnexpectedTopic   InfoHash

    -- | Some trackers or DHT can return 'PeerId' of a peer. If a
    -- remote peer handshaked with different 'hsPeerId' then this
    -- exception is raised. Can occur in 'connectWire' only.
  | UnexpectedPeerId  PeerId

    -- | Accepted peer have sent unknown torrent infohash in
    -- 'hsInfoHash' field. This situation usually happen when /this/
    -- peer have deleted the requested torrent. The error can occur in
    -- 'acceptWire' function only.
  | UnknownTopic      InfoHash

    -- | A remote peer have 'ExtExtended' enabled but did not send an
    -- 'ExtendedHandshake' back.
  | HandshakeRefused

    -- | 'Network.BitTorrent.Exchange.Message.Bitfield' message MUST
    -- be send either once or zero times, but either this peer or
    -- remote peer send a bitfield message the second time.
  | BitfieldAlreadySent ChannelSide

    -- | Capabilities violation. For example this exception can occur
    -- when a peer have sent 'Port' message but 'ExtDHT' is not
    -- allowed in 'connCaps'.
  | DisallowedMessage
    { -- | Who sent invalid message.
      violentSender     :: ChannelSide

      -- | If the 'violentSender' reconnect with this extension
      -- enabled then he can try to send this message.
    , extensionRequired :: Extension
    }
    deriving Show

instance Pretty ProtocolError where
  pretty = PP.text . show

errorPenalty :: ProtocolError -> Int
errorPenalty (InvalidProtocol      _) = 1
errorPenalty (UnexpectedProtocol   _) = 1
errorPenalty (UnexpectedTopic      _) = 1
errorPenalty (UnexpectedPeerId     _) = 1
errorPenalty (UnknownTopic         _) = 0
errorPenalty (HandshakeRefused      ) = 1
errorPenalty (BitfieldAlreadySent  _) = 1
errorPenalty (DisallowedMessage  _ _) = 1

-- | Exceptions used to interrupt the current P2P session.
data WireFailure
  = ConnectionRefused IOError

    -- | Force termination of wire connection.
    --
    --   Normally you should throw only this exception from event loop
    --   using 'disconnectPeer', other exceptions are thrown
    --   automatically by functions from this module.
    --
  | DisconnectPeer

     -- | A peer not responding and did not send a 'KeepAlive' message
     -- for a specified period of time.
  | PeerDisconnected

    -- | A remote peer have sent some unknown message we unable to
    -- parse.
  | DecodingError GetException

    -- | See 'ProtocolError' for more details.
  | ProtocolError ProtocolError

    -- | A possible malicious peer have sent too many control messages
    -- without making any progress.
  | FloodDetected ConnectionStats
    deriving (Show, Typeable)

instance Exception WireFailure

instance Pretty WireFailure where
  pretty = PP.text . show

-- TODO
-- data Penalty = Ban | Penalty Int

peerPenalty :: WireFailure -> Int
peerPenalty  DisconnectPeer   = 0
peerPenalty  PeerDisconnected = 0
peerPenalty (DecodingError _) = 1
peerPenalty (ProtocolError e) = errorPenalty e
peerPenalty (FloodDetected _) = 1

-- | Do nothing with exception, used with 'handle' or 'try'.
isWireFailure :: Monad m => WireFailure -> m ()
isWireFailure _ = return ()

protocolError :: MonadThrow m => ProtocolError -> m a
protocolError = monadThrow . ProtocolError

{-----------------------------------------------------------------------
--  Stats
-----------------------------------------------------------------------}

-- | Message stats in one direction.
data FlowStats = FlowStats
  { -- | Number of the messages sent or received.
    messageCount :: {-# UNPACK #-} !Int
    -- | Sum of byte sequences of all messages.
  , messageBytes :: {-# UNPACK #-} !ByteStats
  } deriving Show

instance Pretty FlowStats where
  pretty FlowStats {..} =
    PP.int messageCount <+> "messages" $+$
    pretty messageBytes

-- | Zeroed stats.
instance Default FlowStats where
  def = FlowStats 0 def

-- | Monoid under addition.
instance Monoid FlowStats where
  mempty = def
  mappend a b = FlowStats
    { messageBytes = messageBytes a <> messageBytes b
    , messageCount = messageCount a +  messageCount b
    }

-- | Find average length of byte sequences per message.
avgByteStats :: FlowStats -> ByteStats
avgByteStats (FlowStats n ByteStats {..}) = ByteStats
  { overhead = overhead `quot` n
  , control  = control  `quot` n
  , payload  = payload  `quot` n
  }

-- | Message stats in both directions. This data can be retrieved
-- using 'getStats' function.
--
--   Note that this stats is completely different from
--   'Data.Torrent.Progress.Progress': payload bytes not necessary
--   equal to downloaded\/uploaded bytes since a peer can send a
--   broken block.
--
data ConnectionStats = ConnectionStats
  { -- | Received messages stats.
    incomingFlow  :: !FlowStats
    -- | Sent messages stats.
  , outcomingFlow :: !FlowStats
  } deriving Show

instance Pretty ConnectionStats where
  pretty ConnectionStats {..} = vcat
    [ "Recv:" <+> pretty incomingFlow
    , "Sent:" <+> pretty outcomingFlow
    , "Both:" <+> pretty (incomingFlow <> outcomingFlow)
    ]

-- | Zeroed stats.
instance Default ConnectionStats where
  def = ConnectionStats def def

-- | Monoid under addition.
instance Monoid ConnectionStats where
  mempty = def
  mappend a b = ConnectionStats
    { incomingFlow  = incomingFlow  a <> incomingFlow  b
    , outcomingFlow = outcomingFlow a <> outcomingFlow b
    }

-- | Aggregate one more message stats in the /specified/ direction.
addStats :: ChannelSide -> ByteStats -> ConnectionStats -> ConnectionStats
addStats ThisPeer   x s = s { outcomingFlow = (FlowStats 1 x) <> (outcomingFlow s) }
addStats RemotePeer x s = s { incomingFlow  = (FlowStats 1 x) <> (incomingFlow  s) }

-- | Sum of overhead and control bytes in both directions.
wastedBytes :: ConnectionStats -> Int
wastedBytes ConnectionStats {..} = overhead + control
  where
    FlowStats _ ByteStats {..} = incomingFlow <> outcomingFlow

-- | Sum of payload bytes in both directions.
payloadBytes :: ConnectionStats -> Int
payloadBytes ConnectionStats {..} =
  payload (messageBytes (incomingFlow <> outcomingFlow))

-- | Sum of any bytes in both directions.
transmittedBytes :: ConnectionStats -> Int
transmittedBytes ConnectionStats {..} =
  byteLength (messageBytes (incomingFlow <> outcomingFlow))

{-----------------------------------------------------------------------
--  Flood protection
-----------------------------------------------------------------------}

defaultFloodFactor :: Int
defaultFloodFactor = 1

-- | This is a very permissive value, connection setup usually takes
-- around 10-100KB, including both directions.
defaultFloodThreshold :: Int
defaultFloodThreshold = 2 * 1024 * 1024

-- | A flood detection function.
type Detector stats = Int   -- ^ Factor;
                   -> Int   -- ^ Threshold;
                   -> stats -- ^ Stats to analyse;
                   -> Bool  -- ^ Is this a flooded connection?

defaultDetector :: Detector ConnectionStats
defaultDetector factor threshold s =
  transmittedBytes s     > threshold &&
  factor * wastedBytes s > payloadBytes s

-- | Flood detection is used to protect /this/ peer against a /remote/
-- malicious peer sending meaningless control messages.
data FloodDetector = FloodDetector
  { -- | Max ratio of payload bytes to control bytes.
    floodFactor    :: {-# UNPACK #-} !Int

    -- | Max count of bytes connection /setup/ can take including
    -- 'Handshake', 'ExtendedHandshake', 'Bitfield', 'Have' and 'Port'
    -- messages. This value is used to avoid false positives at the
    -- connection initialization.
  , floodThreshold :: {-# UNPACK #-} !Int

    -- | Flood predicate on the /current/ 'ConnectionStats'.
  , floodPredicate :: Detector ConnectionStats
  } deriving Show

-- | Flood detector with very permissive options.
instance Default FloodDetector where
  def = FloodDetector
    { floodFactor    = defaultFloodFactor
    , floodThreshold = defaultFloodThreshold
    , floodPredicate = defaultDetector
    }

-- | This peer might drop connection if the detector gives positive answer.
runDetector :: FloodDetector -> ConnectionStats -> Bool
runDetector FloodDetector {..} = floodPredicate floodFactor floodThreshold

{-----------------------------------------------------------------------
--  Options
-----------------------------------------------------------------------}

-- | Various connection settings and limits.
data Options = Options
  { -- | How often /this/ peer should send 'KeepAlive' messages.
    keepaliveInterval   :: {-# UNPACK #-} !Int

    -- | /This/ peer will drop connection if a /remote/ peer did not
    -- send any message for this period of time.
  , keepaliveTimeout    :: {-# UNPACK #-} !Int

    -- | Used to protect against flood attacks.
  , floodDetector       :: FloodDetector

    -- | Used to protect against flood attacks in /metadata
    -- exchange/. Normally, a requesting peer should request each
    -- 'InfoDict' piece only one time, but a malicious peer can
    -- saturate wire with 'MetadataRequest' messages thus flooding
    -- responding peer.
    --
    --   This value set upper bound for number of 'MetadataRequests'
    --   for each piece.
    --
  , metadataFactor      :: {-# UNPACK #-} !Int

    -- | Used to protect against out-of-memory attacks: malicious peer
    -- can claim that 'totalSize' is, say, 100TB and send some random
    -- data instead of infodict pieces. Since requesting peer unable
    -- to check not completed infodict via the infohash, the
    -- accumulated pieces will allocate the all available memory.
    --
    --   This limit set upper bound for 'InfoDict' size. See
    --   'ExtendedMetadata' for more info.
    --
  , maxInfoDictSize     :: {-# UNPACK #-} !Int
  } deriving Show

-- | Permissive default parameters, most likely you don't need to
-- change them.
instance Default Options where
  def = Options
    { keepaliveInterval = defaultKeepAliveInterval
    , keepaliveTimeout  = defaultKeepAliveTimeout
    , floodDetector     = def
    , metadataFactor    = defaultMetadataFactor
    , maxInfoDictSize   = defaultMaxInfoDictSize
    }

{-----------------------------------------------------------------------
--  Connection
-----------------------------------------------------------------------}

data Cached a = Cached { unCache :: a, cached :: BS.ByteString }

cache :: (BEncode a) => a -> Cached a
cache s = Cached s (BSL.toStrict $ BE.encode s)

data ConnectionState = ConnectionState {
    -- | If @not (allowed ExtExtended connCaps)@ then this set is always
    -- empty. Otherwise it has the BEP10 extension protocol mandated mapping of
    -- 'MessageId' to the message type for the remote peer.
    _connExtCaps      :: !ExtendedCaps

    -- | Current extended handshake information from the remote peer
  , _connRemoteEhs    :: !ExtendedHandshake

    -- | Various stats about messages sent and received. Stats can be
    -- used to protect /this/ peer against flood attacks.
  , _connStats        :: !ConnectionStats

  , _connStatus       :: !ConnectionStatus

    -- | Bitfield of remote endpoint.
  , _connBitfield     :: !Bitfield

    -- | Infodict associated with this Connection's connTopic.
  , _connMetadata     :: Maybe (Cached InfoDict)
  }

makeLenses ''ConnectionState

-- | Connection keep various info about both peers.
data Connection s = Connection
  { connRemoteAddr   :: !(PeerAddr IP)

    -- | /Both/ peers handshaked with this protocol string. The only
    -- value is \"Bittorrent Protocol\" but this can be changed in
    -- future.
  , connProtocol     :: !ProtocolName

    -- | Set of enabled core extensions, i.e. the pre BEP10 extension
    -- mechanism. This value is used to check if a message is allowed
    -- to be sent or received.
  , connCaps         :: !Caps

    -- | /Both/ peers handshaked with this infohash. A connection can
    -- handle only one topic, use 'reconnect' to change the current
    -- topic.
  , connTopic        :: !InfoHash

    -- | Typically extracted from handshake.
  , connRemotePeerId :: !PeerId

    -- | Typically extracted from handshake.
  , connThisPeerId   :: !PeerId

    -- |
  , connOptions      :: !Options

    -- | Mutable connection state, see 'ConnectionState'
  , connState        :: !(IORef ConnectionState)

--    -- | Max request queue length.
--  , connMaxQueueLen  :: !Int

    -- | Environment data.
  , connSession      :: !s

  , connChan         :: !(Chan Message)
  }

instance Pretty (Connection s) where
  pretty Connection {..} = "Connection"

instance ToLogStr (Connection s) where
  toLogStr Connection {..} = mconcat
    [ toLogStr (show connRemoteAddr)
    , toLogStr (show connProtocol)
    , toLogStr (show connCaps)
    , toLogStr (show connTopic)
    , toLogStr (show connRemotePeerId)
    , toLogStr (show connThisPeerId)
    , toLogStr (show connOptions)
    ]

-- TODO check extended messages too
isAllowed :: Connection s -> Message -> Bool
isAllowed Connection {..} msg
  | Just ext <- requires msg = ext `allowed` connCaps
  |          otherwise       = True

{-----------------------------------------------------------------------
--  Hanshaking
-----------------------------------------------------------------------}

sendHandshake :: Socket -> Handshake -> IO ()
sendHandshake sock hs = sendAll sock (S.encode hs)

recvHandshake :: Socket -> IO Handshake
recvHandshake sock = do
    header <- BS.recv sock 1
    unless (BS.length header == 1) $
      throw $ userError "Unable to receive handshake header."

    let protocolLen = BS.head header
    let restLen     = handshakeSize protocolLen - 1

    body <- BS.recv sock restLen
    let resp = BS.cons protocolLen body
    either (throwIO . userError) return $ S.decode resp

-- | Handshaking with a peer specified by the second argument.
--
--   It's important to send handshake first because /accepting/ peer
--   do not know handshake topic and will wait until /connecting/ peer
--   will send handshake.
--
initiateHandshake :: Socket -> Handshake -> IO Handshake
initiateHandshake sock hs = do
  sendHandshake sock hs
  recvHandshake sock

{-----------------------------------------------------------------------
--  Wire
-----------------------------------------------------------------------}

-- | do not expose this so we can change it without breaking api
newtype Connected s m a = Connected { runConnected :: (ReaderT (Connection s) m a) }
    deriving (Functor, Applicative, Monad
             , MonadIO, MonadReader (Connection s), MonadThrow
             )

instance MonadIO m => MonadState ConnectionState (Connected s m) where
    get   = Connected (asks connState) >>= liftIO . readIORef
    put x = Connected (asks connState) >>= liftIO . flip writeIORef x

instance MonadTrans (Connected s) where
    lift = Connected . lift

-- | A duplex channel connected to a remote peer which keep tracks
-- connection parameters.
type Wire s a = ConduitM Message Message (Connected s IO) a

{-----------------------------------------------------------------------
--  Query
-----------------------------------------------------------------------}

setExtCaps :: ExtendedCaps -> Wire s ()
setExtCaps x = lift $ connExtCaps .= x

-- | Get current extended capabilities. Note that this value can
-- change in current session if either this or remote peer will
-- initiate rehandshaking.
getExtCaps :: Wire s ExtendedCaps
getExtCaps = lift $ use connExtCaps

setRemoteEhs :: ExtendedHandshake -> Wire s ()
setRemoteEhs x = lift $ connRemoteEhs .= x

getRemoteEhs :: Wire s ExtendedHandshake
getRemoteEhs = lift $ use connRemoteEhs

-- | Get current stats. Note that this value will change with the next
-- sent or received message.
getStats :: Wire s ConnectionStats
getStats = lift $ use connStats

-- | See the 'Connection' section for more info.
getConnection :: Wire s (Connection s)
getConnection = lift ask

getSession :: Wire s s
getSession = lift (asks connSession)

-- TODO configurable
defQueueLength :: Int
defQueueLength = 1

getAdvertisedQueueLength :: Wire s Int
getAdvertisedQueueLength = do
  ExtendedHandshake {..} <- getRemoteEhs
  return $ fromMaybe defQueueLength ehsQueueLength

{-----------------------------------------------------------------------
--  Wrapper
-----------------------------------------------------------------------}

putStats :: ChannelSide -> Message -> Connected s IO ()
putStats side msg = connStats %= addStats side (stats msg)

validate :: ChannelSide -> Message -> Connected s IO ()
validate side msg = do
  caps <- asks connCaps
  case requires msg of
    Nothing  -> return ()
    Just ext
      | ext `allowed` caps -> return ()
      |     otherwise      -> protocolError $ DisallowedMessage side ext

trackFlow :: ChannelSide -> Wire s ()
trackFlow side = iterM $ do
  validate side
  putStats side

{-----------------------------------------------------------------------
--  Setup
-----------------------------------------------------------------------}

-- System.Timeout.timeout multiplier
seconds :: Int
seconds = 1000000

sinkChan :: MonadIO m => Chan Message -> Sink Message m ()
sinkChan chan = await >>= maybe (return ()) (liftIO . writeChan chan)

sourceChan :: MonadIO m => Int -> Chan Message -> Source m Message
sourceChan interval chan = do
  mmsg <- liftIO $ timeout (interval * seconds) $ readChan chan
  yield $ fromMaybe Msg.KeepAlive mmsg

-- | Normally you should use 'connectWire' or 'acceptWire'.
runWire :: Wire s () -> Socket -> Chan Message -> Connection s -> IO ()
runWire action sock chan conn = flip runReaderT conn $ runConnected $
  sourceSocket sock        $=
    conduitGet S.get       $=
      trackFlow RemotePeer $=
         action            $=
      trackFlow ThisPeer   $$
    sinkChan chan

-- | This function will block until a peer send new message. You can
-- also use 'await'.
recvMessage :: Wire s Message
recvMessage = await >>= maybe (monadThrow PeerDisconnected) return

-- | You can also use 'yield'.
sendMessage :: PeerMessage msg => msg -> Wire s ()
sendMessage msg = do
  ecaps <- use connExtCaps
  yield $ envelop ecaps msg

-- | Filter pending messages from send buffer.
filterQueue :: (Message -> Bool) -> Wire s ()
filterQueue p = lift $ do
  chan <- asks connChan
  liftIO $ getChanContents chan >>= writeList2Chan chan . L.filter p

-- | Forcefully terminate wire session and close socket.
disconnectPeer :: Wire s a
disconnectPeer = monadThrow DisconnectPeer

extendedHandshake :: ExtendedCaps -> Wire s ()
extendedHandshake caps = do
  -- TODO add other params to the handshake
  sendMessage $ nullExtendedHandshake caps
  msg <- recvMessage
  case msg of
    Extended (EHandshake remoteEhs@(ExtendedHandshake {..})) -> do
      setExtCaps $ ehsCaps <> caps
      setRemoteEhs remoteEhs
    _ -> protocolError HandshakeRefused

rehandshake :: ExtendedCaps -> Wire s ()
rehandshake caps = undefined

reconnect :: Wire s ()
reconnect = undefined

-- | Initiate 'Wire' connection and handshake with a peer. This function will
-- also do the BEP10 extension protocol handshake if 'ExtExtended' is enabled on
-- both sides.
--
-- This function can throw 'WireFailure' exception.
--
connectWire :: s -> Handshake -> PeerAddr IP -> ExtendedCaps -> Chan Message
            -> Wire s () -> IO ()
connectWire session hs addr extCaps chan wire = do
  let catchRefusal m = try m >>= either (throwIO . ConnectionRefused) return
  bracket (catchRefusal (peerSocket Stream addr)) close $ \ sock -> do
    hs' <- initiateHandshake sock hs

    Prelude.mapM_ (\(t,e) -> unless t $ throwIO $ ProtocolError e) [
      (def == hsProtocol hs'
      , InvalidProtocol $ hsProtocol hs'),
      (hsProtocol hs == hsProtocol hs'
      , UnexpectedProtocol $ hsProtocol hs'),
      (hsInfoHash hs == hsInfoHash hs'
      , UnexpectedTopic $ hsInfoHash hs'),
      (hsPeerId hs' == fromMaybe (hsPeerId hs') (peerId addr)
      , UnexpectedPeerId $ hsPeerId hs')
      ]

    let caps = hsReserved hs <> hsReserved hs'
        wire' = if ExtExtended `allowed` caps
                then extendedHandshake extCaps >> wire
                else wire

    cstate <- newIORef $ ConnectionState {
        _connExtCaps      = def
      , _connRemoteEhs    = def
      , _connStats        = ConnectionStats {
                             outcomingFlow = FlowStats 1 $ handshakeStats hs
                           , incomingFlow  = FlowStats 1 $ handshakeStats hs'
                           }
      , _connStatus       = def
      , _connBitfield     = BF.haveNone 0
      , _connMetadata     = Nothing
      }

    -- TODO make KA interval configurable
    let kaInterval = defaultKeepAliveInterval
    bracket
      (forkIO $ sourceChan kaInterval chan $= conduitPut S.put $$ sinkSocket sock)
      (killThread) $ \ _ ->
      runWire wire' sock chan $ Connection
        { connRemoteAddr   = addr
        , connProtocol     = hsProtocol hs
        , connCaps         = caps
        , connTopic        = hsInfoHash hs
        , connRemotePeerId = hsPeerId   hs'
        , connThisPeerId   = hsPeerId   hs
        , connOptions      = def
        , connState        = cstate
        , connSession      = session
        , connChan         = chan
        }

-- | Accept 'Wire' connection using already 'Network.Socket.accept'ed
--   socket. For peer listener loop the 'acceptSafe' should be
--   prefered against 'accept'. The socket will be closed at exit.
--
--   This function can throw 'WireFailure' exception.
--
acceptWire :: Socket -> PeerAddr IP -> Wire s () -> IO ()
acceptWire sock peerAddr wire = do
  bracket (return sock) close $ \ _ -> do
    error "acceptWire: not implemented"

{-----------------------------------------------------------------------
--  Connection Status
-----------------------------------------------------------------------}

getStatus :: Wire s ConnectionStatus
getStatus = lift $ use connStatus

updateConnStatus :: ChannelSide -> StatusUpdate -> Wire s ()
updateConnStatus side u = lift $ do
    connStatus %= (over (statusSide side) (updateStatus u))
  where
    statusSide ThisPeer   = clientStatus
    statusSide RemotePeer = remoteStatus

getRemoteBitfield :: Wire s Bitfield
getRemoteBitfield = lift $ use connBitfield

updateRemoteBitfield :: (Bitfield -> Bitfield) -> Wire s ()
updateRemoteBitfield f = lift $ connBitfield %= f

-- | Used when size of bitfield becomes known.
resizeBitfield :: Int -> Wire s ()
resizeBitfield n = updateRemoteBitfield (adjustSize n)

{-----------------------------------------------------------------------
--  Metadata exchange
-----------------------------------------------------------------------}
-- TODO introduce new metadata exchange specific exceptions

fetchMetadata :: Wire s [BS.ByteString]
fetchMetadata = loop 0
  where
    recvData = recvMessage >>= inspect
      where
        inspect (Extended (EMetadata _ meta)) =
          case meta of
            MetadataRequest pix -> do
              sendMessage (MetadataReject pix)
              recvData
            MetadataData   {..} -> return (piece, totalSize)
            MetadataReject   _  -> disconnectPeer
            MetadataUnknown   _ -> recvData
        inspect _ = recvData

    loop i = do
      sendMessage (MetadataRequest i)
      (piece, totalSize) <- recvData
      unless (pieceIndex piece == i) $ do
        disconnectPeer

      if piece `isLastPiece` totalSize
        then pure [pieceData piece]
        else (pieceData piece :) <$> loop (succ i)

getMetadata :: Wire s InfoDict
getMetadata = do
  chunks <- fetchMetadata
  Connection {..} <- getConnection
  case BE.decode (BS.concat chunks) of
     Right (infodict @ InfoDict {..})
       | connTopic == idInfoHash -> return infodict
       |         otherwise       -> error "broken infodict"
     Left err -> error $ "unable to parse infodict" ++ err
