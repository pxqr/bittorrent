{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.BitTorrent.Exchange.Session
       (
       ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Data.Function
import Data.IORef
import Data.Ord
import Data.Typeable
import Text.PrettyPrint

import Data.Torrent.Bitfield
import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Message
import Network.BitTorrent.Exchange.Status


type Extension = ()

data ExchangeError
  = InvalidPieceIx PieceIx
  | InvalidBlock   BlockIx
  | CorruptedPiece PieceIx

-- | Peer session contain all data necessary for peer to peer
-- communication.
data ExchangeSession = ExchangeSession
  { -- | Used as unique identifier of the session.
    connectedPeerAddr :: !PeerAddr

    -- | Extensions such that both peer and client support.
  , enabledExtensions :: [Extension]

    -- | Broadcast messages waiting to be sent to peer.
  , pendingMessages    :: !(TChan   Message)

    -- | Dymanic P2P data.
  , sessionState       :: !(IORef  SessionState)
  }

instance Eq ExchangeSession where
  (==) = (==) `on` connectedPeerAddr
  {-# INLINE (==) #-}

instance Ord ExchangeSession where
  compare = comparing connectedPeerAddr
  {-# INLINE compare #-}

enqueueBroadcast :: ExchangeSession -> Message -> IO ()
enqueueBroadcast = undefined

dequeueBroadcast :: ExchangeSession -> IO Message
dequeueBroadcast = undefined

{-----------------------------------------------------------------------
--  Session state
-----------------------------------------------------------------------}

data SessionState = SessionState
  { _bitfield :: !Bitfield      -- ^ Other peer Have bitfield.
  , _status   :: !SessionStatus -- ^ Status of both peers.
  } deriving (Show, Eq)

$(makeLenses ''SessionState)

--initialSessionState :: PieceCount -> SessionState
--initialSessionState pc = SessionState (haveNone pc) def

--getSessionState :: PeerSession -> IO SessionState
--getSessionState PeerSession {..} = readIORef sessionState

{-
{-----------------------------------------------------------------------
--  Broadcasting: Have, Cancel, Bitfield, SuggestPiece
-----------------------------------------------------------------------}
{-
Here we should enqueue broadcast messages and keep in mind that:
  * We should enqueue broadcast events as they are appear.
  * We should yield broadcast messages as fast as we get them.

these 2 phases might differ in time significantly

**TODO**: do this; but only when it'll be clean which other broadcast
messages & events we should send.

1. Update client have bitfield --\____ in one transaction;
2. Update downloaded stats     --/
3. Signal to the all other peer about this.
-}

available :: Bitfield -> SwarmSession -> STM ()
available bf SwarmSession {..} = {-# SCC available #-} do
    updateProgress >> broadcast
  where
    updateProgress = do
      let piLen = ciPieceLength $ tInfo $ torrentMeta
      let bytes = piLen * BF.haveCount bf
      modifyTVar' (currentProgress clientSession) (downloadedProgress bytes)

    broadcast = mapM_ (writeTChan broadcastMessages . Have) (BF.toList bf)

-- TODO compute size of messages: if it's faster to send Bitfield
-- instead many Have do that

-- Also if there is single Have message in queue then the
-- corresponding piece is likely still in memory or disc cache,
-- when we can send SuggestPiece.

readAvail :: TChan a -> STM [a]
readAvail chan = do
  m <- tryReadTChan chan
  case m of
    Just a  -> (:) <$> pure a <*> readAvail chan
    Nothing -> return []

-- | Get pending messages queue appeared in result of asynchronously
--   changed client state. Resulting queue should be sent to a peer
-- immediately.
--
getPending :: PeerSession -> IO [Message]
getPending PeerSession {..} = {-# SCC getPending #-} do
  atomically (readAvail pendingMessages)
-}