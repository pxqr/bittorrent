{- TODO turn awaitEvent and yieldEvent to sourcePeer and sinkPeer

      sourceSocket sock   $=
        conduitGet S.get  $=
          sourcePeer      $=
            p2p           $=
          sinkPeer        $=
        conduitPut S.put  $$
      sinkSocket sock

       measure performance
 -}

-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides P2P communication and aims to hide the
--   following stuff under the hood:
--
--     * TODO;
--
--     * /keep alives/ -- ;
--
--     * /choking mechanism/ -- is used ;
--
--     * /message broadcasting/ -- ;
--
--     * /message filtering/ -- due to network latency and concurrency
--     some arriving messages might not make sense in the current
--     session context;
--
--     * /scatter\/gather pieces/ -- ;
--
--     * /various P2P protocol extensions/ -- .
--
--   Finally we get a simple event-based communication model.
--
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE BangPatterns               #-}
module Network.BitTorrent.Exchange
       ( P2P
       , runP2P
       , spawnP2P

         -- * Query
       , getHaveCount
       , getWantCount
       , getPieceCount
       , peerOffer

         -- * Events
       , Event(..)
       , awaitEvent
       , yieldEvent
       , handleEvent

         -- * Exceptions
       , disconnect
       , protocolError

         -- * Block
       , Block(..), BlockIx(..)
       ) where

import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource

import Data.IORef
import Data.Conduit as C
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize as S
import Text.PrettyPrint as PP hiding (($$))


import Network


import Network.BitTorrent.Internal
import Network.BitTorrent.Extension
import Network.BitTorrent.Peer
import Network.BitTorrent.Exchange.Protocol
import Data.Bitfield as BF


{-----------------------------------------------------------------------
    Peer wire
-----------------------------------------------------------------------}

type PeerWire = ConduitM Message Message IO

runPeerWire :: Socket -> PeerWire () -> IO ()
runPeerWire sock p2p =
  sourceSocket sock   $=
    conduitGet S.get  $=
      p2p             $=
    conduitPut S.put  $$
  sinkSocket sock

awaitMessage :: P2P Message
awaitMessage = P2P $ ReaderT $ const $ {-# SCC awaitMessage #-} go
  where
    go = await >>= maybe (monadThrow PeerDisconnected) return
{-# INLINE awaitMessage #-}

yieldMessage :: Message -> P2P ()
yieldMessage msg =  P2P $ ReaderT $ const $ {-# SCC yieldMessage #-} C.yield msg
{-# INLINE yieldMessage #-}

flushPending :: P2P ()
flushPending = {-# SCC flushPending #-} do
  se <- ask
  q  <- liftIO (getPending se)
  -- TODO send vectored
  mapM_ yieldMessage q

{-----------------------------------------------------------------------
    P2P monad
-----------------------------------------------------------------------}

-- |
--   Exceptions:
--
--     * SessionException: is visible only within one peer
--     session. Use this exception to terminate P2P session, but not
--     the swarm session.
--
newtype P2P a = P2P {
    unP2P :: ReaderT PeerSession PeerWire a
  } deriving ( Functor, Applicative, Monad
             , MonadIO, MonadThrow, MonadActive
             , MonadReader PeerSession
             )

instance MonadState SessionState P2P where
  {-# SPECIALIZE instance MonadState SessionState P2P #-}
  get    = asks sessionState >>= liftIO . readIORef
  {-# INLINE get #-}
  put !s = asks sessionState >>= \ref -> liftIO $ writeIORef ref s
  {-# INLINE put #-}

runSession :: SwarmSession -> PeerAddr -> P2P () -> IO ()
runSession  se addr p2p =
    handle isIOException $
      withPeerSession se addr $ \(sock, pses) -> do
        runPeerWire sock (runReaderT (unP2P p2p) pses)
  where
    isIOException :: IOException -> IO ()
    isIOException _ = return ()

-- | Run P2P session in the current thread. Normally you don't need this
-- function in client application, except for debugging.
runP2P :: SwarmSession -> PeerAddr -> P2P () -> IO ()
runP2P se addr p2p = waitVacancy se $ runSession se addr p2p

-- | Run P2P session in forked thread. Might be used in listener or
-- some other loop. Note that this function may block while waiting
-- for a vacant place: use forkIO and runP2P instead.
spawnP2P :: SwarmSession -> PeerAddr -> P2P () -> IO ThreadId
spawnP2P se addr p2p = do
  enterSwarm se
  forkIO $ do
    runSession se addr p2p `finally` leaveSwarm se

-- TODO unify this all using PeerConnection
{-
listenP2P :: SwarmSession -> P2P () -> IO PortNumber
listenP2P _ _ = undefined

chainP2P :: SwarmSession -> PeerConnection -> P2P () -> IO ()
-}
{-----------------------------------------------------------------------
    Exceptions
-----------------------------------------------------------------------}

-- | Terminate the current 'P2P' session.
disconnect :: P2P a
disconnect = monadThrow PeerDisconnected

-- TODO handle all protocol details here so we can hide this from
-- public interface |
protocolError ::  Doc -> P2P a
protocolError = monadThrow . ProtocolError

{-----------------------------------------------------------------------
    Helpers
-----------------------------------------------------------------------}

getClientBF :: P2P Bitfield
getClientBF = asks swarmSession >>= liftIO . getClientBitfield
{-# INLINE getClientBF #-}

-- | Count of client /have/ pieces.
getHaveCount :: P2P PieceCount
getHaveCount = haveCount <$> getClientBF
{-# INLINE getHaveCount #-}

-- | Count of client do not /have/ pieces.
getWantCount :: P2P PieceCount
getWantCount = totalCount <$> getClientBF
{-# INLINE getWantCount #-}

-- | Count of both /have/ and /want/ pieces.
getPieceCount :: P2P PieceCount
getPieceCount = asks findPieceCount
{-# INLINE getPieceCount #-}

-- for internal use only
emptyBF :: P2P Bitfield
emptyBF = liftM haveNone getPieceCount

fullBF ::  P2P Bitfield
fullBF = liftM haveAll getPieceCount

singletonBF :: PieceIx -> P2P Bitfield
singletonBF i = liftM (BF.singleton i) getPieceCount

adjustBF :: Bitfield -> P2P Bitfield
adjustBF bf = (`adjustSize` bf) `liftM` getPieceCount


peerWant   :: P2P Bitfield
peerWant   = BF.difference <$> getClientBF  <*> use bitfield

clientWant :: P2P Bitfield
clientWant = BF.difference <$> use bitfield <*> getClientBF

peerOffer :: P2P Bitfield
peerOffer = do
  sessionStatus <- use status
  if canDownload sessionStatus then clientWant else emptyBF

clientOffer :: P2P Bitfield
clientOffer = do
  sessionStatus <- use status
  if canUpload sessionStatus then peerWant else emptyBF



revise :: P2P Bitfield
revise = do
  want <- clientWant
  let peerInteresting = not (BF.null want)
  clientInterested <- use (status.clientStatus.interested)

  when (clientInterested /= peerInteresting) $ do
    yieldMessage $ if peerInteresting then Interested else NotInterested
    status.clientStatus.interested .= peerInteresting

  return want

requireExtension :: Extension -> P2P ()
requireExtension required = do
  enabled <- asks enabledExtensions
  unless (required `elem` enabled) $
    protocolError $ ppExtension required <+> "not enabled"

--    haveMessage bf = do
--      cbf <- undefined -- liftIO $ readIORef $ clientBitfield swarmSession
--      if undefined -- ix `member` bf
--        then nextEvent se
--        else undefined  -- return $ Available diff

{-----------------------------------------------------------------------
    Exchange
-----------------------------------------------------------------------}


-- | The 'Event' occur when either client or a peer change their
-- state. 'Event' are similar to 'Message' but differ in. We could
-- both wait for an event or raise an event using the 'awaitEvent' and
-- 'yieldEvent' functions respectively.
--
--
--   'awaitEvent'\/'yieldEvent' properties:
--
--     * between any await or yield state of the (another)peer could not change.
--
data Event
    -- | Generalize 'Bitfield', 'Have', 'HaveAll', 'HaveNone',
    -- 'SuggestPiece', 'AllowedFast' messages.
  = Available Bitfield

    -- | Generalize 'Request' and 'Interested' messages.
  | Want      BlockIx

    -- | Generalize 'Piece' and 'Unchoke' messages.
  | Fragment  Block
    deriving Show

-- INVARIANT:
--
--   * Available Bitfield is never empty
--

-- | You could think of 'awaitEvent' as wait until something interesting occur.
--
--   The following table shows which events may occur:
--
--   > +----------+---------+
--   > | Leacher  |  Seeder |
--   > |----------+---------+
--   > | Available|         |
--   > | Want     |   Want  |
--   > | Fragment |         |
--   > +----------+---------+
--
--   The reason is that seeder is not interested in any piece, and
--   both available or fragment events doesn't make sense in this context.
--
--   Some properties:
--
--     forall (Fragment block). isPiece block == True
--
awaitEvent :: P2P Event
awaitEvent = {-# SCC awaitEvent #-} awaitMessage >>= go
  where
    go KeepAlive = awaitEvent
    go Choke     = do
      status.peerStatus.choking .= True
      awaitEvent

    go Unchoke   = do
      status.peerStatus.choking .= False
      offer <- peerOffer
      if BF.null offer
        then awaitEvent
        else return (Available offer)

    go Interested    = do
      status.peerStatus.interested .= True
      awaitEvent

    go NotInterested = do
      status.peerStatus.interested .= False
      awaitEvent

    go (Have idx)      = do
      bitfield %= have idx
      _ <- revise

      offer <- peerOffer
      if not (BF.null offer)
        then return (Available offer)
        else awaitEvent

    go (Bitfield bf)  = do
      new <- adjustBF bf
      bitfield .= new
      _ <- revise

      offer <- peerOffer
      if not (BF.null offer)
        then return (Available offer)
        else awaitEvent

    go (Request  bix) = do
      bf <- clientOffer
      if ixPiece bix `BF.member` bf
         then return (Want bix)
         else do
-- check if extension is enabled
--           yieldMessage (RejectRequest bix)
           awaitEvent

    go (Piece    blk) = do
      -- this protect us from malicious peers and duplication
      wanted <- clientWant
      if blkPiece blk `BF.member` wanted
        then return (Fragment blk)
        else awaitEvent

    go (Cancel _) = do
      error "cancel message not implemented"

    go (Port     _) = do
      requireExtension ExtDHT
      error "port message not implemented"

    go HaveAll = do
      requireExtension ExtFast
      bitfield <~ fullBF
      _ <- revise
      awaitEvent

    go HaveNone = do
      requireExtension ExtFast
      bitfield <~ emptyBF
      _ <- revise
      awaitEvent

    go (SuggestPiece idx) = do
      requireExtension ExtFast
      bf <- use bitfield
      if idx `BF.notMember` bf
        then Available <$> singletonBF idx
        else awaitEvent

    go (RejectRequest _) = do
       requireExtension ExtFast
       awaitEvent

    go (AllowedFast _) = do
       requireExtension ExtFast
       awaitEvent

-- TODO minimize number of peerOffer calls

-- | Raise an events which may occur
--
--   This table shows when a some specific events /makes sense/ to yield:
--
--   @
--   +----------+---------+
--   | Leacher  |  Seeder |
--   |----------+---------+
--   | Available|         |
--   | Want     |Fragment |
--   | Fragment |         |
--   +----------+---------+
--   @
--
--   Seeder should not yield:
--
--     * Available -- seeder could not store anything new.
--
--     * Want -- seeder alread have everything, no reason to want.
--
--   Hovewer, it's okay to not obey the rules -- if we are yield some
--   event which doesn't /makes sense/ in the current context then it
--   most likely will be ignored without any network IO.
--
yieldEvent  :: Event -> P2P ()
yieldEvent e = {-# SCC yieldEvent #-} go e
  where
    go (Available ixs) = asks swarmSession >>= liftIO . available ixs
    go (Want      bix) = do
      offer <- peerOffer
      if ixPiece bix `BF.member` offer
        then yieldMessage (Request bix)
        else return ()

    go (Fragment  blk) = do
      offer <- clientOffer
      if blkPiece blk `BF.member` offer
        then yieldMessage (Piece blk)
        else return ()


handleEvent :: (Event -> P2P Event) -> P2P ()
handleEvent action = awaitEvent >>= action >>= yieldEvent
