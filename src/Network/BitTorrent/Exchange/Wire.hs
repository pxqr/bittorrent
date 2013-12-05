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
       , ProtocolError (..)
       , WireFailure   (..)
       , isWireFailure
       , disconnectPeer

         -- ** Connection
       , Connection    (..)

         -- ** Setup
       , runWire
       , connectWire
       , acceptWire

         -- ** Query
       , getConnection
       , getExtCaps
       ) where

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
{-----------------------------------------------------------------------
--  Exceptions
-----------------------------------------------------------------------}

data ChannelSide
  = ThisPeer
  | RemotePeer
    deriving (Show, Eq, Enum)

-- TODO pretty instance

-- | Errors occur when a remote peer violates protocol specification.
data ProtocolError
  = UnexpectedTopic   InfoHash -- ^ peer replied with unexpected infohash.
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
--  Connection
-----------------------------------------------------------------------}

data Connection = Connection
  { connCaps         :: !Caps
  , connExtCaps      :: !(IORef ExtendedCaps)
  , connTopic        :: !InfoHash
  , connRemotePeerId :: !PeerId
  , connThisPeerId   :: !PeerId
  }

instance Pretty Connection where
  pretty Connection {..} = "Connection"

isAllowed :: Connection -> Message -> Bool
isAllowed Connection {..} msg
  | Just ext <- requires msg = allowed connCaps ext
  |          otherwise       = True

{-----------------------------------------------------------------------
--  Hanshaking
-----------------------------------------------------------------------}

-- | TODO remove socket stuff to corresponding module
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

getExtCaps :: Wire ExtendedCaps
getExtCaps = do
  capsRef <- lift $ asks connExtCaps
  liftIO $ readIORef capsRef

setExtCaps :: ExtendedCaps -> Wire ()
setExtCaps caps = do
  capsRef <- lift $ asks connExtCaps
  liftIO $ writeIORef capsRef caps

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
          | allowed caps ext -> yield msg
          |     otherwise    -> protocolError $ InvalidMessage side ext

validateBoth :: Wire () -> Wire ()
validateBoth action = do
  validate RemotePeer
  action
  validate ThisPeer

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
recvMessage = undefined

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

    unless (hsInfoHash hs == hsInfoHash hs') $ do
      throwIO $ ProtocolError $ UnexpectedTopic (hsInfoHash hs')

    unless (hsPeerId hs' == fromMaybe (hsPeerId hs') (peerID addr)) $ do
      throwIO $ ProtocolError $ UnexpectedPeerId (hsPeerId hs')

    let caps = hsReserved hs <> hsReserved hs'
    let wire' = if allowed caps ExtExtended
                then extendedHandshake extCaps >> wire
                else wire

    extCapsRef <- newIORef def
    runWire wire' sock $ Connection
      { connCaps         = caps
      , connExtCaps      = extCapsRef
      , connTopic        = hsInfoHash hs
      , connRemotePeerId = hsPeerId   hs'
      , connThisPeerId   = hsPeerId   hs
      }

acceptWire :: Wire () -> Socket -> IO ()
acceptWire = undefined
