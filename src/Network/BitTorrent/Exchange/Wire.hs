-- |
--
--   Message flow
--   Duplex channell
--   This module control /integrity/ of data send and received.
--

{-# LANGUAGE DeriveDataTypeable #-}
module Network.BitTorrent.Exchange.Wire
       ( -- * Exception
         ProtocolError (..)
       , WireFailure   (..)
       , isWireFailure

         -- * Wire
       , Connection    (..)
       , Wire
       , runWire
       , connectWire
       , acceptWire
       ) where

import Control.Exception
import Control.Monad.Reader
import Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Cereal as S
import Data.Conduit.Network
import Data.Default
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
import Network.BitTorrent.Core.PeerId
import Network.BitTorrent.Core.PeerAddr
import Network.BitTorrent.Exchange.Message


{-----------------------------------------------------------------------
--  Exceptions
-----------------------------------------------------------------------}

data ChannelSide
  = ThisPeer
  | RemotePeer
    deriving (Show, Eq, Enum)

-- | Errors occur when a remote peer violates protocol specification.
data ProtocolError
  = UnexpectedTopic   InfoHash -- ^ peer replied with unexpected infohash.
  | UnexpectedPeerId  PeerId   -- ^ peer replied with unexpected peer id.
  | UnknownTopic      InfoHash -- ^ peer requested unknown torrent.
  | InvalidMessage
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
  , connExtCaps      :: !ExtendedCaps -- TODO caps can be enabled during communication
  , connTopic        :: !InfoHash
  , connRemotePeerId :: !PeerId
  , connThisPeerId   :: !PeerId
  } deriving Show

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

validate :: Wire ()
validate = do
  mmsg <- await
  case mmsg of
    Nothing  -> return ()
    Just msg -> do
      valid <- lift $ asks (`isAllowed` msg)
      if valid then yield msg else error "TODO"


runWire :: Wire () -> Socket -> Connection -> IO ()
runWire action sock = runReaderT $
  sourceSocket sock     $=
    S.conduitGet S.get  $=
      action            $=
    S.conduitPut S.put  $$
  sinkSocket sock

sendMessage :: PeerMessage msg => msg -> Wire ()
sendMessage msg = do
  ecaps <- lift $ asks connExtCaps
  yield $ envelop ecaps msg

recvMessage :: Wire Message
recvMessage = undefined



extendedHandshake :: Wire ()
extendedHandshake = undefined

connectWire :: Handshake -> PeerAddr -> ExtendedCaps -> Wire () -> IO ()
connectWire hs addr caps wire =
  bracket (connectToPeer addr) close $ \ sock -> do
    hs' <- initiateHandshake sock hs

    unless (hsInfoHash hs == hsInfoHash hs') $ do
      throwIO $ ProtocolError $ UnexpectedTopic (hsInfoHash hs')

    unless (hsPeerId hs' == fromMaybe (hsPeerId hs') (peerID addr)) $ do
      throwIO $ ProtocolError $ UnexpectedPeerId (hsPeerId hs')

    let caps = hsReserved hs <> hsReserved hs'
    if allowed caps ExtExtended
      then return () else return ()

    runWire wire sock $ Connection
      { connCaps         = caps
      , connExtCaps      = def
      , connTopic        = hsInfoHash hs
      , connRemotePeerId = hsPeerId   hs'
      , connThisPeerId   = hsPeerId   hs
      }

acceptWire :: Wire () -> Socket -> IO ()
acceptWire = undefined
