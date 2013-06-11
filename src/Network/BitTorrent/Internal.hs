-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module implement opaque broadcast message passing. It
--   provides sessions needed by Network.BitTorrent and
--   Network.BitTorrent.Exchange and modules. To hide some internals
--   of this module we detach it from Exchange.
--
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Internal
       ( Progress(..), startProgress
       , ClientSession(..), newClient
       , SwarmSession(..), newLeacher, newSeeder
       , PeerSession(..), withPeerSession

         -- * Timeouts
       , updateIncoming, updateOutcoming
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

import Data.IORef
import Data.Function
import Data.Ord
import Data.Set as S

import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize

import Network
import Network.Socket
import Network.Socket.ByteString

import GHC.Event as Ev

import Data.Bitfield as BF
import Data.Torrent
import Network.BitTorrent.Extension
import Network.BitTorrent.Peer
import Network.BitTorrent.Exchange.Protocol as BT



-- | 'Progress' contains upload/download/left stats about
--   current client state.
--
--   This data is considered as dynamic within one session.
--
data Progress = Progress {
    prUploaded   :: Integer -- ^ Total amount of bytes uploaded.
  , prDownloaded :: Integer -- ^ Total amount of bytes downloaded.
  , prLeft       :: Integer -- ^ Total amount of bytes left.
  } deriving Show

startProgress :: Integer -> Progress
startProgress = Progress 0 0


{-----------------------------------------------------------------------
    Client session
-----------------------------------------------------------------------}

-- | In one application you could have many clients.
data ClientSession = ClientSession {
    clientPeerID      :: PeerID      -- ^
  , allowedExtensions :: [Extension] -- ^
  , swarmSessions     :: TVar (Set SwarmSession)
  , eventManager      :: EventManager
  , currentProgress   :: IORef Progress
  }

instance Eq ClientSession where
  (==) = (==) `on` clientPeerID

instance Ord ClientSession where
  compare = comparing clientPeerID

newClient :: [Extension] -> IO ClientSession
newClient exts = ClientSession <$> newPeerID
                               <*> pure exts
                               <*> newTVarIO S.empty
                               <*> Ev.new
                               <*> newIORef (startProgress 0)

{-----------------------------------------------------------------------
    Swarm session
-----------------------------------------------------------------------}

-- | Extensions are set globally by
--   Swarm session are un
data SwarmSession = SwarmSession {
    torrentMeta       :: Torrent
  , clientSession     :: ClientSession
  , clientBitfield    :: IORef Bitfield
  , connectedPeers    :: TVar (Set PeerSession)
  }

instance Eq SwarmSession where
  (==) = (==) `on` (tInfoHash . torrentMeta)

instance Ord SwarmSession where
  compare = comparing (tInfoHash . torrentMeta)

newSwarmSession :: Bitfield -> ClientSession -> Torrent -> IO SwarmSession
newSwarmSession bf cs @ ClientSession {..} t @ Torrent {..}
  = SwarmSession <$> pure t
                 <*> pure cs
                 <*> newIORef bf
                 <*> newTVarIO S.empty

newSeeder :: ClientSession -> Torrent -> IO SwarmSession
newSeeder cs t @ Torrent {..}
  = newSwarmSession (haveAll (pieceCount tInfo)) cs t

newLeacher :: ClientSession -> Torrent -> IO SwarmSession
newLeacher cs t @ Torrent {..}
  = newSwarmSession (haveNone (pieceCount tInfo)) cs t

isLeacher :: SwarmSession -> IO Bool
isLeacher = undefined

{-----------------------------------------------------------------------
    Peer session
-----------------------------------------------------------------------}

data PeerSession = PeerSession {
    connectedPeerAddr :: PeerAddr
  , swarmSession      :: SwarmSession
  , enabledExtensions :: [Extension]

    -- | To dissconnect from died peers appropriately we should check
    -- if a peer do not sent the KA message within given interval. If
    -- yes, we should throw an exception in 'TimeoutCallback' and
    -- close session between peers.
    --
    -- We should update timeout if we /receive/ any message within
    -- timeout interval to keep connection up.
  , incomingTimeout     :: TimeoutKey

    -- | To send KA message appropriately we should know when was last
    -- time we sent a message to a peer. To do that we keep registered
    -- timeout in event manager and if we do not sent any message to
    -- the peer within given interval then we send KA message in
    -- 'TimeoutCallback'.
    --
    -- We should update timeout if we /send/ any message within timeout
    -- to avoid reduntant KA messages.
  , outcomingTimeout   :: TimeoutKey

  , broadcastMessages :: Chan   [Message]
  , peerBitfield      :: IORef  Bitfield
  , peerSessionStatus :: IORef  SessionStatus
  }

instance Eq PeerSession where
  (==) = (==) `on` connectedPeerAddr

instance Ord PeerSession where
  compare = comparing connectedPeerAddr

-- TODO check if it connected yet peer
withPeerSession :: SwarmSession -> PeerAddr
                -> ((Socket, PeerSession) -> IO a)
                -> IO a

withPeerSession ss @ SwarmSession {..} addr
    = bracket openSession closeSession
  where
    openSession = do
      let caps = encodeExts $ allowedExtensions $ clientSession
      let pid  = clientPeerID $ clientSession
      let chs  = Handshake defaultBTProtocol caps (tInfoHash torrentMeta) pid

      sock <- connectToPeer addr
      phs  <- handshake sock chs `onException` close sock

      let enabled = decodeExts (enabledCaps caps (handshakeCaps phs))
      ps <- PeerSession addr ss enabled
         <$> registerTimeout (eventManager clientSession)
                maxIncomingTime abortSession
         <*> registerTimeout (eventManager clientSession)
                maxOutcomingTime (sendKA sock)
         <*> newChan
         <*> pure clientBitfield
         <*> newIORef initSessionStatus
      return (sock, ps)

    closeSession = close . fst

{-----------------------------------------------------------------------
    Timeouts
-----------------------------------------------------------------------}

sec :: Int
sec = 1000 * 1000

maxIncomingTime :: Int
maxIncomingTime = 120 * sec

maxOutcomingTime :: Int
maxOutcomingTime = 60 * sec

-- | Should be called after we have received any message from a peer.
updateIncoming :: PeerSession -> IO ()
updateIncoming PeerSession {..} = do
  updateTimeout (eventManager (clientSession swarmSession))
    incomingTimeout maxIncomingTime

-- | Should be called before we have send any message to a peer.
updateOutcoming :: PeerSession -> IO ()
updateOutcoming PeerSession {..}  =
  updateTimeout (eventManager (clientSession swarmSession))
    outcomingTimeout maxOutcomingTime

sendKA :: Socket -> IO ()
sendKA sock = sendAll sock (encode BT.KeepAlive)

abortSession :: IO ()
abortSession = error "abortSession: not implemented"
