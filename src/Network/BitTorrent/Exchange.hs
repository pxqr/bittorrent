-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE BangPatterns               #-}
module Network.BitTorrent.Exchange
       ( -- * Block
         Block(..), BlockIx(..)

         -- * Event
       , Event(..)

       , P2P
       , runP2P, spawnP2P
       , awaitEvent, yieldEvent

       , disconnect, protocolError

       , getHaveCount
       , getWantCount
       , getPieceCount
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


data Event = Available Bitfield
           | Want      BlockIx
           | Fragment  Block
             deriving Show

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
awaitMessage = P2P (ReaderT (const go))
  where
    go = await >>= maybe (monadThrow PeerDisconnected) return
{-# INLINE awaitMessage #-}

yieldMessage :: Message -> P2P ()
yieldMessage msg = P2P $ ReaderT $ \se -> C.yield msg
{-# INLINE yieldMessage #-}

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

-- |
--   > +----------+---------+
--   > | Leacher  |  Seeder |
--   > |----------+---------+
--   > | Available|         |
--   > | Want     |   Want  |
--   > | Fragment |         |
--   > +----------+---------+
--
--
--   properties:
--
--   forall (Fragment block). isPiece block == True
--
awaitEvent :: P2P Event
awaitEvent = awaitMessage >>= go
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
      new <- singletonBF idx
      bitfield %= BF.union new
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

-- TODO minimized number of peerOffer calls

-- |
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
yieldEvent  :: Event -> P2P ()
yieldEvent (Available _  ) = undefined
yieldEvent (Want      bix) = do
  offer <- peerOffer
  if ixPiece bix `BF.member` offer
     then yieldMessage (Request bix)
     else return ()

yieldEvent (Fragment  blk) = do
  offer <- clientOffer
  if blkPiece blk `BF.member` offer
    then yieldMessage (Piece blk)
    else return ()

--flushBroadcast :: P2P ()
--flushBroadcast = nextBroadcast >>= maybe (return ()) go
--  where
--    go = undefined

checkPiece :: PieceLIx -> {-ByteString -> -} P2P Bool
checkPiece = undefined