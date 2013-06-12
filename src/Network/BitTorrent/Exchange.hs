-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DoAndIfThenElse            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
module Network.BitTorrent.Exchange
       ( -- * Block
         Block(..), BlockIx(..)

         -- * Event
       , Event(..)

       , P2P, withPeer
       , awaitEvent, yieldEvent
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource

import Data.IORef
import Data.Function
import Data.Ord
import Data.Set as S

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
import Data.Torrent


data Event = Available Bitfield
           | Want      BlockIx
           | Fragment  Block
             deriving Show

{-----------------------------------------------------------------------
    Peer wire
-----------------------------------------------------------------------}

type PeerWire = ConduitM Message Message IO

runConduit :: Socket -> PeerWire () -> IO ()
runConduit sock p2p =
  sourceSocket sock   $=
    conduitGet S.get  $=
      forever p2p     $=
    conduitPut S.put  $$
  sinkSocket sock

awaitMessage :: P2P Message
awaitMessage = P2P (ReaderT go)
  where
    go se = do
      liftIO $ putStrLn "trying recv:"
      mmsg <- await
      case mmsg of
        Nothing  -> monadThrow SessionException
        Just msg -> do
--          liftIO $ updateIncoming se
          liftIO $ print ("recv:" <+> ppMessage msg)
          return msg

yieldMessage :: Message -> P2P ()
yieldMessage msg = P2P $ ReaderT $ \se -> do
  C.yield msg
  liftIO $ print $ "sent:" <+> ppMessage msg
  liftIO $ updateOutcoming se


peerWant :: P2P Bitfield
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
    sessionError $ ppExtension required <+> "not enabled"

--    haveMessage bf = do
--      cbf <- undefined -- liftIO $ readIORef $ clientBitfield swarmSession
--      if undefined -- ix `member` bf
--        then nextEvent se
--        else undefined  -- return $ Available diff


-- |
--   +----------+---------+
--   | Leacher  |  Seeder |
--   |----------+---------+
--   | Available|         |
--   | Want     |   Want  |
--   | Fragment |         |
--   +----------+---------+
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

    go (Have ix)      = do
      new <- singletonBF ix
      bitfield %= BF.union new
      revise

      offer <- peerOffer
      if not (BF.null offer)
        then return (Available offer)
        else awaitEvent

    go (Bitfield bf)  = do
      new <- adjustBF bf
      bitfield .= new
      revise

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

{-
    go (Port     _) = do
      requireExtension ExtDHT
      undefined

    go HaveAll = do
      requireExtension ExtFast
      bitfield <~ fullBF
      revise
      awaitEvent

    go HaveNone = do
      requireExtension ExtFast
      bitfield <~ emptyBF
      revise
      awaitEvent

    go (SuggestPiece ix) = do
      requireExtension ExtFast
      bf <- use bitfield
      if ix `BF.notMember` bf
        then Available <$> singletonBF ix
        else awaitEvent

    go (RejectRequest ix) = do
       requireExtension ExtFast
       awaitMessage

    go (AllowedFast pix) =
       requireExtension ExtFast
       awaitMessage
-}


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
yieldEvent (Available bf)  = undefined
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

{-----------------------------------------------------------------------
    P2P monad
-----------------------------------------------------------------------}

-- |
--   Exceptions:
--
--     * SessionException: is visible with one peer session. Use this
--     exception to terminate P2P session, but not the swarm session.
--
newtype P2P a = P2P {
    runP2P :: ReaderT PeerSession PeerWire a
  } deriving ( Functor, Applicative, Monad
             , MonadIO, MonadThrow, MonadActive
             , MonadReader PeerSession
             )

withPeer :: SwarmSession -> PeerAddr -> P2P () -> IO ()
withPeer se addr p2p =
  withPeerSession se addr $ \(sock, pses) -> do
    handle putSessionException $
      runConduit sock (runReaderT (runP2P p2p) pses)
