-- |
--   Module      :  Network.BitTorrent.Exchange.Wire
--   Copyright   :  (c) Sam Truzjan 2013
--                  (c) Daniel Gröber 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Each peer wire connection is identified by triple @(topic,
--   remote_addr, this_addr)@. This means that connections are the
--   same if and only if their 'ConnectionId' are the same. Of course,
--   you /must/ avoid duplicated connections.
--
--   This module control /integrity/ of data send and received.
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.BitTorrent.Exchange.Connection
       ( -- * Wire
         Connected
       , Wire
       , ChannelSide   (..)

         -- * Connection
       , Connection
       , connInitiatedBy

         -- ** Identity
       , connRemoteAddr
       , connTopic
       , connRemotePeerId
       , connThisPeerId

         -- ** Capabilities
       , connProtocol
       , connCaps
       , connExtCaps
       , connRemoteEhs

         -- ** State
       , connStatus
       , connBitfield

         -- ** Env
       , connOptions
       , connSession
       , connStats

         -- ** Status
       , PeerStatus (..)
       , ConnectionStatus (..)
       , updateStatus
       , statusUpdates
       , clientStatus
       , remoteStatus
       , canUpload
       , canDownload
       , defaultUnchokeSlots
       , defaultRechokeInterval


         -- * Setup
       , ConnectionPrefs  (..)
       , SessionLink      (..)
       , ConnectionConfig (..)

         -- ** Initiate
       , connectWire

         -- ** Accept
       , PendingConnection
       , newPendingConnection
       , pendingPeer
       , pendingCaps
       , pendingTopic
       , closePending
       , acceptWire

         -- ** Post setup actions
       , resizeBitfield

         -- * Messaging
       , recvMessage
       , sendMessage
       , filterQueue
       , getMaxQueueLength

         -- * Exceptions
       , ProtocolError (..)
       , WireFailure   (..)
       , peerPenalty
       , isWireFailure
       , disconnectPeer

         -- * Stats
       , ByteStats       (..)
       , FlowStats       (..)
       , ConnectionStats (..)

         -- * Flood detection
       , FloodDetector   (..)

         -- * Options
       , Options         (..)
       ) where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Control.Lens
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Conduit as C
import Data.Conduit.Cereal
import Data.Conduit.List
import Data.Conduit.Network
import Data.Default
import Data.IORef
import Data.List as L
import Data.Maybe as M
import Data.Monoid
import Data.Serialize as S
import Data.Typeable
import Network
import Network.Socket hiding (Connected)
import Network.Socket.ByteString as BS
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class
import Text.Show.Functions ()
import System.Log.FastLogger (ToLogStr(..))
import System.Timeout

import Data.Torrent
import Network.BitTorrent.Address
import Network.BitTorrent.Exchange.Bitfield as BF
import Network.BitTorrent.Exchange.Message  as Msg

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
    -- 'connectWire' or 'acceptWire' only.
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

instance Eq FloodDetector where
  a == b = floodFactor    a == floodFactor    b
        && floodThreshold a == floodThreshold b

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

  , requestQueueLength  :: {-# UNPACK #-} !Int

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
  } deriving (Show, Eq)

-- | Permissive default parameters, most likely you don't need to
-- change them.
instance Default Options where
  def = Options
    { keepaliveInterval  = defaultKeepAliveInterval
    , keepaliveTimeout   = defaultKeepAliveTimeout
    , requestQueueLength = defaultRequestQueueLength
    , floodDetector      = def
    , metadataFactor     = defaultMetadataFactor
    , maxInfoDictSize    = defaultMaxInfoDictSize
    }

{-----------------------------------------------------------------------
--  Peer status
-----------------------------------------------------------------------}

-- | Connections contain two bits of state on either end: choked or
-- not, and interested or not.
data PeerStatus = PeerStatus
  { -- | Choking is a notification that no data will be sent until
    -- unchoking happens.
    _choking    :: !Bool

    -- |
  , _interested :: !Bool
  } deriving (Show, Eq, Ord)

$(makeLenses ''PeerStatus)

instance Pretty PeerStatus where
  pretty PeerStatus {..} =
    pretty (Choking _choking) <+> "and" <+> pretty (Interested _interested)

-- | Connections start out choked and not interested.
instance Default PeerStatus where
  def = PeerStatus True False

instance Monoid PeerStatus where
  mempty      = def
  mappend a b = PeerStatus
    { _choking    = _choking    a && _choking    b
    , _interested = _interested a || _interested b
    }

-- | Can be used to update remote peer status using incoming 'Status'
-- message.
updateStatus :: StatusUpdate -> PeerStatus -> PeerStatus
updateStatus (Choking    b) = choking    .~ b
updateStatus (Interested b) = interested .~ b

-- | Can be used to generate outcoming messages.
statusUpdates :: PeerStatus -> PeerStatus -> [StatusUpdate]
statusUpdates a b = M.catMaybes $
  [ if _choking    a == _choking    b then Nothing
    else Just $ Choking    $ _choking    b
  , if _interested a == _interested b then Nothing
    else Just $ Interested $ _interested b
  ]

{-----------------------------------------------------------------------
--  Connection status
-----------------------------------------------------------------------}

-- | Status of the both endpoints.
data ConnectionStatus = ConnectionStatus
  { _clientStatus :: !PeerStatus
  , _remoteStatus :: !PeerStatus
  } deriving (Show, Eq)

$(makeLenses ''ConnectionStatus)

instance Pretty ConnectionStatus where
  pretty ConnectionStatus {..} =
    "this  " PP.<+> pretty _clientStatus PP.$$
    "remote" PP.<+> pretty _remoteStatus

-- | Connections start out choked and not interested.
instance Default ConnectionStatus where
  def = ConnectionStatus def def

-- | Can the client transfer to the remote peer?
canUpload :: ConnectionStatus -> Bool
canUpload ConnectionStatus {..}
  = _interested _remoteStatus && not (_choking _clientStatus)

-- | Can the client transfer from the remote peer?
canDownload :: ConnectionStatus -> Bool
canDownload ConnectionStatus {..}
  = _interested _clientStatus && not (_choking _remoteStatus)

-- | Indicates how many peers are allowed to download from the client
-- by default.
defaultUnchokeSlots :: Int
defaultUnchokeSlots = 4

-- |
defaultRechokeInterval :: Int
defaultRechokeInterval = 10 * 1000 * 1000

{-----------------------------------------------------------------------
--  Connection
-----------------------------------------------------------------------}

data ConnectionState = ConnectionState {
    -- | If @not (allowed ExtExtended connCaps)@ then this set is always
    -- empty. Otherwise it has the BEP10 extension protocol mandated mapping of
    -- 'MessageId' to the message type for the remote peer.
    --
    --  Note that this value can change in current session if either
    --  this or remote peer will initiate rehandshaking.
    --
    _connExtCaps      :: !ExtendedCaps

    -- | Current extended handshake information from the remote peer
  , _connRemoteEhs    :: !ExtendedHandshake

    -- | Various stats about messages sent and received. Stats can be
    -- used to protect /this/ peer against flood attacks.
    --
    -- Note that this value will change with the next sent or received
    -- message.
  , _connStats        :: !ConnectionStats

  , _connStatus       :: !ConnectionStatus

    -- | Bitfield of remote endpoint.
  , _connBitfield     :: !Bitfield
  }

makeLenses ''ConnectionState

instance Default ConnectionState where
   def = ConnectionState
     { _connExtCaps      = def
     , _connRemoteEhs    = def
     , _connStats        = def
     , _connStatus       = def
     , _connBitfield     = BF.haveNone 0
     }

-- | Connection keep various info about both peers.
data Connection s = Connection
  { connInitiatedBy  :: !ChannelSide

  , connRemoteAddr   :: !(PeerAddr IP)

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

data HandshakePair = HandshakePair
  { handshakeSent :: !Handshake
  , handshakeRecv :: !Handshake
  } deriving (Show, Eq)

validatePair :: HandshakePair -> PeerAddr IP -> IO ()
validatePair (HandshakePair hs hs') addr = Prelude.mapM_ checkProp
  [ (def            == hsProtocol hs', InvalidProtocol    $ hsProtocol hs')
  , (hsProtocol hs  == hsProtocol hs', UnexpectedProtocol $ hsProtocol hs')
  , (hsInfoHash hs  == hsInfoHash hs', UnexpectedTopic    $ hsInfoHash hs')
  , (hsPeerId   hs' == fromMaybe (hsPeerId hs') (peerId addr)
    , UnexpectedPeerId $ hsPeerId hs')
  ]
  where
    checkProp (t, e) = unless t $ throwIO $ ProtocolError e

-- | Connection state /right/ after handshaking.
establishedStats :: HandshakePair -> ConnectionStats
establishedStats HandshakePair {..} = ConnectionStats
  { outcomingFlow = FlowStats 1 $ handshakeStats handshakeSent
  , incomingFlow  = FlowStats 1 $ handshakeStats handshakeRecv
  }

{-----------------------------------------------------------------------
--  Wire
-----------------------------------------------------------------------}

-- | do not expose this so we can change it without breaking api
newtype Connected s a = Connected { runConnected :: (ReaderT (Connection s) IO a) }
    deriving (Functor, Applicative, Monad
             , MonadIO, MonadReader (Connection s), MonadThrow
             )

instance MonadState ConnectionState (Connected s) where
    get   = Connected (asks connState) >>= liftIO . readIORef
    put x = Connected (asks connState) >>= liftIO . flip writeIORef x

-- | A duplex channel connected to a remote peer which keep tracks
-- connection parameters.
type Wire s a = ConduitM Message Message (Connected s) a

{-----------------------------------------------------------------------
--  Wrapper
-----------------------------------------------------------------------}

putStats :: ChannelSide -> Message -> Connected s ()
putStats side msg = connStats %= addStats side (stats msg)

validate :: ChannelSide -> Message -> Connected s ()
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
      trackFlow ThisPeer   C.$$
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

getMaxQueueLength :: Connected s Int
getMaxQueueLength = do
  advertisedLen <- ehsQueueLength <$> use connRemoteEhs
  defaultLen    <- asks (requestQueueLength . connOptions)
  return $ fromMaybe defaultLen advertisedLen

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
      connExtCaps   .= (ehsCaps <> caps)
      connRemoteEhs .= remoteEhs
    _ -> protocolError HandshakeRefused

rehandshake :: ExtendedCaps -> Wire s ()
rehandshake caps = error "rehandshake"

reconnect :: Wire s ()
reconnect = error "reconnect"

data ConnectionId    = ConnectionId
  { topic      :: !InfoHash
  , remoteAddr :: !(PeerAddr IP)
  , thisAddr   :: !(PeerAddr (Maybe IP)) -- ^ foreign address of this node.
  }

-- | /Preffered/ settings of wire. To get the real use 'ask'.
data ConnectionPrefs = ConnectionPrefs
  { prefOptions  :: !Options
  , prefProtocol :: !ProtocolName
  , prefCaps     :: !Caps
  , prefExtCaps  :: !ExtendedCaps
  } deriving (Show, Eq)

instance Default ConnectionPrefs where
  def = ConnectionPrefs
    { prefOptions  = def
    , prefProtocol = def
    , prefCaps     = def
    , prefExtCaps  = def
    }

normalize :: ConnectionPrefs -> ConnectionPrefs
normalize = error "normalize"

-- | Bridge between 'Connection' and 'Network.BitTorrent.Exchange.Session'.
data SessionLink s = SessionLink
  { linkTopic        :: !(InfoHash)
  , linkPeerId       :: !(PeerId)
  , linkMetadataSize :: !(Maybe Int)
  , linkOutputChan   :: !(Maybe (Chan Message))
  , linkSession      :: !(s)
  }

data ConnectionConfig s = ConnectionConfig
  { cfgPrefs   :: !(ConnectionPrefs)
  , cfgSession :: !(SessionLink s)
  , cfgWire    :: !(Wire s ())
  }

configHandshake :: ConnectionConfig s -> Handshake
configHandshake ConnectionConfig {..} = Handshake
  { hsProtocol = prefProtocol  cfgPrefs
  , hsReserved = prefCaps      cfgPrefs
  , hsInfoHash = linkTopic     cfgSession
  , hsPeerId   = linkPeerId    cfgSession
  }

{-----------------------------------------------------------------------
--  Pending connections
-----------------------------------------------------------------------}

-- | Connection in half opened state. A normal usage scenario:
--
--    * Opened using 'newPendingConnection', usually in the listener
--    loop;
--
--    * Closed using 'closePending' if 'pendingPeer' is banned,
--    'pendingCaps' is prohibited or pendingTopic is unknown;
--
--    * Accepted using 'acceptWire' otherwise.
--
data PendingConnection = PendingConnection
  { pendingSock  :: Socket
  , pendingPeer  :: PeerAddr IP -- ^ 'peerId' is always non empty;
  , pendingCaps  :: Caps        -- ^ advertised by the peer;
  , pendingTopic :: InfoHash    -- ^ possible non-existent topic.
  }

-- | Reconstruct handshake sent by the remote peer.
pendingHandshake :: PendingConnection -> Handshake
pendingHandshake PendingConnection {..} = Handshake
  { hsProtocol = def
  , hsReserved = pendingCaps
  , hsInfoHash = pendingTopic
  , hsPeerId   = fromMaybe (error "pendingHandshake: impossible")
                           (peerId pendingPeer)
  }

-- |
--
--   This function can throw 'WireFailure' exception.
--
newPendingConnection :: Socket -> PeerAddr IP -> IO PendingConnection
newPendingConnection sock addr = do
  Handshake {..} <- recvHandshake sock
  unless (hsProtocol == def) $ do
    throwIO $ ProtocolError $ InvalidProtocol hsProtocol
  return PendingConnection
    { pendingSock  = sock
    , pendingPeer  = addr { peerId = Just hsPeerId }
    , pendingCaps  = hsReserved
    , pendingTopic = hsInfoHash
    }

-- | Release all resources associated with the given connection. Note
-- that you /must not/ 'closePending' if you 'acceptWire'.
closePending :: PendingConnection -> IO ()
closePending PendingConnection {..} = do
  close pendingSock

{-----------------------------------------------------------------------
--  Connection setup
-----------------------------------------------------------------------}

chanToSock :: Int -> Chan Message -> Socket -> IO ()
chanToSock ka chan sock =
  sourceChan ka chan $= conduitPut S.put C.$$ sinkSocket sock

afterHandshaking :: ChannelSide -> PeerAddr IP -> Socket -> HandshakePair
                 -> ConnectionConfig s -> IO ()
afterHandshaking initiator addr sock
  hpair @ (HandshakePair hs hs')
          (ConnectionConfig
           { cfgPrefs   = ConnectionPrefs {..}
           , cfgSession = SessionLink     {..}
           , cfgWire    = wire
           }) = do
    let caps  = hsReserved hs <> hsReserved hs'
    cstate <- newIORef def { _connStats = establishedStats hpair }
    chan   <- maybe newChan return linkOutputChan
    let conn  = Connection {
          connInitiatedBy  = initiator
        , connRemoteAddr   = addr
        , connProtocol     = hsProtocol hs
        , connCaps         = caps
        , connTopic        = hsInfoHash hs
        , connRemotePeerId = hsPeerId   hs'
        , connThisPeerId   = hsPeerId   hs
        , connOptions      = def
        , connState        = cstate
        , connSession      = linkSession
        , connChan         = chan
        }

    -- TODO make KA interval configurable
    let kaInterval = defaultKeepAliveInterval
        wire' = if ExtExtended `allowed` caps
                then extendedHandshake prefExtCaps >> wire
                else wire

    bracket (forkIO (chanToSock kaInterval chan sock))
            (killThread)
            (\ _ -> runWire wire' sock chan conn)

-- | Initiate 'Wire' connection and handshake with a peer. This function will
-- also do the BEP10 extension protocol handshake if 'ExtExtended' is enabled on
-- both sides.
--
-- This function can throw 'WireFailure' exception.
--
connectWire :: PeerAddr IP -> ConnectionConfig s -> IO ()
connectWire addr cfg = do
  let catchRefusal m = try m >>= either (throwIO . ConnectionRefused) return
  bracket (catchRefusal (peerSocket Stream addr)) close $ \ sock -> do
    let hs = configHandshake cfg
    hs' <- initiateHandshake sock hs
    let hpair = HandshakePair hs hs'
    validatePair hpair addr
    afterHandshaking ThisPeer addr sock hpair cfg

-- | Accept 'Wire' connection using already 'Network.Socket.accept'ed
--   socket. For peer listener loop the 'acceptSafe' should be
--   prefered against 'accept'. The socket will be closed at exit.
--
--   This function can throw 'WireFailure' exception.
--
acceptWire :: PendingConnection -> ConnectionConfig s -> IO ()
acceptWire pc @ PendingConnection {..} cfg = do
  bracket (return pendingSock) close $ \ _ -> do
    unless (linkTopic (cfgSession cfg) == pendingTopic) $ do
      throwIO (ProtocolError (UnexpectedTopic pendingTopic))

    let hs = configHandshake cfg
    sendHandshake pendingSock hs
    let hpair = HandshakePair hs (pendingHandshake pc)

    afterHandshaking RemotePeer pendingPeer pendingSock hpair cfg

-- | Used when size of bitfield becomes known.
resizeBitfield :: Int -> Connected s ()
resizeBitfield n = connBitfield %= adjustSize n
