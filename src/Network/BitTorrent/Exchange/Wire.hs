-- |
--
--   Message flow
--   Duplex channell
--   This module control /integrity/ of data send and received.
--
--
{-# LANGUAGE DeriveDataTypeable #-}
module Network.BitTorrent.Exchange.Wire
       ( -- * Wire
         Wire

         -- ** Exceptions
       , ChannelSide   (..)
       , ProtocolError (..)
       , WireFailure   (..)
       , isWireFailure
       , disconnectPeer

         -- ** Connection
       , Connection
           ( connCaps, connTopic
           , connRemotePeerId, connThisPeerId
           )
       , getConnection

         -- ** Setup
       , runWire
       , connectWire
       , acceptWire

         -- ** Query
       , getExtCaps

         -- ** Messaging
       , validate
       , validateBoth
       , keepStats

         -- ** Stats
       , ConnectionStats (..)
       , getStats
       , askStats

       , recvBytes
       , sentBytes
       , wastedBytes
       , payloadBytes
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Cereal as S
import Data.Conduit.Network
import Data.Default
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Serialize as S
import Data.Typeable
import Network
import Network.Socket
import Network.Socket.ByteString as BS
import Text.PrettyPrint as PP hiding (($$), (<>))
import Text.PrettyPrint.Class

import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Message

-- TODO handle port message?
-- TODO handle limits?
-- TODO filter not requested PIECE messages
-- TODO metadata piece request flood protection
-- TODO piece request flood protection
{-----------------------------------------------------------------------
--  Exceptions
-----------------------------------------------------------------------}

data ChannelSide
  = ThisPeer
  | RemotePeer
    deriving (Show, Eq, Enum, Bounded)

instance Default ChannelSide where
  def = ThisPeer

instance Pretty ChannelSide where
  pretty = PP.text . show

-- | Errors occur when a remote peer violates protocol specification.
data ProtocolError
    -- | Protocol string should be 'BitTorrent Protocol' but remote
    -- peer send a different string.
  = InvalidProtocol   ProtocolString
  | UnexpectedTopic   InfoHash -- ^ peer replied with unexpected infohash.
  | UnexpectedPeerId  PeerId   -- ^ peer replied with unexpected peer id.
  | UnknownTopic      InfoHash -- ^ peer requested unknown torrent.
  | HandshakeRefused           -- ^ peer do not send an extended handshake back.
  | BitfieldAlreadSend ChannelSide
  | InvalidMessage -- TODO caps violation
    { violentSender     :: ChannelSide -- ^ endpoint sent invalid message
    , extensionRequired :: Extension   -- ^
    }
    deriving Show

instance Pretty ProtocolError where
  pretty = PP.text . show

-- | Exceptions used to interrupt the current P2P session.
data WireFailure
  = PeerDisconnected -- ^ A peer not responding.
  | DisconnectPeer   -- ^
  | ProtocolError  ProtocolError
    deriving (Show, Typeable)

instance Exception WireFailure

instance Pretty WireFailure where
  pretty = PP.text . show

-- | Do nothing with exception, used with 'handle' or 'try'.
isWireFailure :: Monad m => WireFailure -> m ()
isWireFailure _ = return ()

{-----------------------------------------------------------------------
--  Stats
-----------------------------------------------------------------------}

type ByteCount = Int

data MessageStats = MessageStats
  { overhead :: {-# UNPACK #-} !ByteCount
  , payload  :: {-# UNPACK #-} !ByteCount
  } deriving Show

instance Default MessageStats where
  def = MessageStats 0 0

instance Monoid MessageStats where
  mempty      = mempty
  mappend a b = MessageStats
    { overhead = overhead a + overhead b
    , payload  = payload  a + payload  b
    }


messageSize :: MessageStats -> Int
messageSize MessageStats {..} = overhead + payload

messageStats :: Message -> MessageStats
messageStats = undefined

data ConnectionStats = ConnectionStats
  { incomingFlow  :: !MessageStats
  , outcomingFlow :: !MessageStats
  } deriving Show

instance Default ConnectionStats where
  def = ConnectionStats def def

addStats :: ChannelSide -> MessageStats -> ConnectionStats -> ConnectionStats
addStats ThisPeer   x s = s { outcomingFlow = outcomingFlow s <> x }
addStats RemotePeer x s = s { incomingFlow  = incomingFlow  s <> x }

recvBytes :: ConnectionStats -> Int
recvBytes = messageSize . incomingFlow

sentBytes :: ConnectionStats -> Int
sentBytes = messageSize . outcomingFlow

wastedBytes :: ConnectionStats -> Int
wastedBytes (ConnectionStats _in out) = overhead _in + overhead out

payloadBytes :: ConnectionStats -> Int
payloadBytes (ConnectionStats _in out) = payload _in + payload out

{-----------------------------------------------------------------------
--  Connection
-----------------------------------------------------------------------}

data Connection = Connection
  { connCaps         :: !Caps
  , connExtCaps      :: !(IORef ExtendedCaps)
  , connTopic        :: !InfoHash
  , connRemotePeerId :: !PeerId
  , connThisPeerId   :: !PeerId
  , connStats        :: !(IORef ConnectionStats)
  }

instance Pretty Connection where
  pretty Connection {..} = "Connection"

isAllowed :: Connection -> Message -> Bool
isAllowed Connection {..} msg
  | Just ext <- requires msg = ext `allowed` connCaps
  |          otherwise       = True

{-----------------------------------------------------------------------
--  Hanshaking
-----------------------------------------------------------------------}

sendHandshake :: Socket -> Handshake -> IO ()
sendHandshake sock hs = sendAll sock (S.encode hs)

-- TODO drop connection if protocol string do not match
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

-- | Tries to connect to peer using reasonable default parameters.
connectToPeer :: PeerAddr -> IO Socket
connectToPeer p = do
  sock <- socket AF_INET Stream Network.Socket.defaultProtocol
  connect sock (peerSockAddr p)
  return sock

{-----------------------------------------------------------------------
--  Wire
-----------------------------------------------------------------------}

type Wire = ConduitM Message Message (ReaderT Connection IO)

protocolError :: ProtocolError -> Wire a
protocolError = monadThrow . ProtocolError

disconnectPeer :: Wire a
disconnectPeer = monadThrow DisconnectPeer

readRef :: (Connection -> IORef a) -> Wire a
readRef f = do
  ref <- lift (asks f)
  liftIO (readIORef ref)

writeRef :: (Connection -> IORef a) -> a -> Wire ()
writeRef f v = do
  ref <- lift (asks f)
  liftIO (writeIORef ref v)

modifyRef :: (Connection -> IORef a) -> (a -> a) -> Wire ()
modifyRef f m = do
  ref <- lift (asks f)
  liftIO (atomicModifyIORef' ref (\x -> (m x, ())))

getExtCaps :: Wire ExtendedCaps
getExtCaps = readRef connExtCaps

setExtCaps :: ExtendedCaps -> Wire ()
setExtCaps = writeRef connExtCaps

getStats :: Wire ConnectionStats
getStats = readRef connStats

askStats :: (ConnectionStats -> a) -> Wire a
askStats f = f <$> getStats

putStats :: ChannelSide -> Message -> Wire ()
putStats side msg = modifyRef connStats (addStats side (messageStats msg))


getConnection :: Wire Connection
getConnection = lift ask

validate :: ChannelSide -> Wire ()
validate side = await >>= maybe (return ()) yieldCheck
  where
    yieldCheck msg = do
      caps <- lift $ asks connCaps
      case requires msg of
        Nothing  -> return ()
        Just ext
          | ext `allowed` caps -> yield msg
          |     otherwise      -> protocolError $ InvalidMessage side ext

validateBoth :: Wire () -> Wire ()
validateBoth action = do
  validate RemotePeer
  action
  validate ThisPeer

keepStats :: Wire ()
keepStats = do
  mmsg <- await
  case mmsg of
    Nothing  -> return ()
    Just msg -> putStats ThisPeer msg -- FIXME not really ThisPeer

runWire :: Wire () -> Socket -> Connection -> IO ()
runWire action sock = runReaderT $
  sourceSocket sock     $=
    S.conduitGet S.get  $=
      action            $=
    S.conduitPut S.put  $$
  sinkSocket sock

sendMessage :: PeerMessage msg => msg -> Wire ()
sendMessage msg = do
  ecaps <- getExtCaps
  yield $ envelop ecaps msg

recvMessage :: Wire Message
recvMessage = await >>= maybe (monadThrow PeerDisconnected) return

extendedHandshake :: ExtendedCaps -> Wire ()
extendedHandshake caps = do
  sendMessage $ nullExtendedHandshake caps
  msg <- recvMessage
  case msg of
    Extended (EHandshake ExtendedHandshake {..}) ->
      setExtCaps $ ehsCaps <> caps
    _ -> protocolError HandshakeRefused

connectWire :: Handshake -> PeerAddr -> ExtendedCaps -> Wire () -> IO ()
connectWire hs addr extCaps wire =
  bracket (connectToPeer addr) close $ \ sock -> do
    hs' <- initiateHandshake sock hs

    unless (def           == hsProtocol hs') $ do
      throwIO $ ProtocolError $ InvalidProtocol (hsProtocol hs')

    unless (hsInfoHash hs == hsInfoHash hs') $ do
      throwIO $ ProtocolError $ UnexpectedTopic (hsInfoHash hs')

    unless (hsPeerId hs' == fromMaybe (hsPeerId hs') (peerId addr)) $ do
      throwIO $ ProtocolError $ UnexpectedPeerId (hsPeerId hs')

    let caps = hsReserved hs <> hsReserved hs'
    let wire' = if ExtExtended `allowed` caps
                then extendedHandshake extCaps >> wire
                else wire

    extCapsRef <- newIORef def
    statsRef   <- newIORef def
    runWire wire' sock $ Connection
      { connCaps         = caps
      , connExtCaps      = extCapsRef
      , connTopic        = hsInfoHash hs
      , connRemotePeerId = hsPeerId   hs'
      , connThisPeerId   = hsPeerId   hs
      , connStats        = statsRef
      }

acceptWire :: Wire () -> Socket -> IO ()
acceptWire = undefined
