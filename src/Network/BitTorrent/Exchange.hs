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
       , awaitEvent, signalEvent
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

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

waitMessage :: P2P Message
waitMessage = P2P (ReaderT go)
  where
    go se = do
      mmsg <- await
      case mmsg of
        Nothing -> go se
        Just msg -> do
          liftIO $ updateIncoming se
          liftIO $ print msg
          return msg

signalMessage :: Message -> P2P ()
signalMessage msg = P2P $ ReaderT $ \se -> do
  C.yield msg
  liftIO $ updateOutcoming se


peerWant :: P2P Bitfield
peerWant   = BF.difference <$> getPeerBF    <*> use bitfield

clientWant :: P2P Bitfield
clientWant = BF.difference <$> use bitfield <*> getPeerBF

peerOffer :: P2P Bitfield
peerOffer = do
  sessionStatus <- use status
  if canDownload sessionStatus then clientWant else emptyBF

clientOffer :: P2P Bitfield
clientOffer = do
  sessionStatus <- use status
  if canUpload sessionStatus then peerWant else emptyBF

revise :: P2P ()
revise = do
  peerInteresting  <- (not . BF.null) <$> clientWant
  clientInterested <- use (status.clientStatus.interested)

  when (clientInterested /= peerInteresting) $
    signalMessage $ if peerInteresting then Interested else NotInterested

requireExtension :: Extension -> P2P ()
requireExtension required = do
  enabled <- asks enabledExtensions
  unless (required `elem` enabled) $
    sessionError $ ppExtension required <+> "not enabled"

peerHave :: P2P Event
peerHave = undefined

--    haveMessage bf = do
--      cbf <- undefined -- liftIO $ readIORef $ clientBitfield swarmSession
--      if undefined -- ix `member` bf
--        then nextEvent se
--        else undefined  -- return $ Available diff


-- |
--   properties:
--
--   forall (Fragment block). isPiece block == True
--
awaitEvent :: P2P Event
awaitEvent = waitMessage >>= go
  where
    go KeepAlive = awaitEvent
    go Choke     = do
      status.peerStatus.choking .= True
      awaitEvent

    go Unchoke   = do
      status.clientStatus.choking .= False
      awaitEvent

    go Interested    = do
      status.peerStatus.interested .= True
      awaitEvent

    go NotInterested = do
      status.peerStatus.interested .= False
      awaitEvent

--    go (Have ix)      = peerHave =<< singletonBF ix
--    go (Bitfield bf)  = peerHave =<< adjustBF bf
    go (Request  bix) = do
      bf <- use bitfield
      if ixPiece bix `BF.member` bf
         then return (Want bix)
         else do
           signalMessage (RejectRequest bix)
           awaitEvent

    go (Piece    blk) = undefined

{-
    go msg @ (Port     _)
      = checkExtension msg ExtDHT $ do
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

    go msg @ (RejectRequest ix)
          = checkExtension msg ExtFast $ do
            undefined

    go msg @ (AllowedFast pix)
          = checkExtension msg ExtFast $ do
            undefined
-}

signalEvent  :: Event -> P2P ()
signalEvent (Available bf) = undefined
signalEvent _ = undefined

--flushBroadcast :: P2P ()
--flushBroadcast = nextBroadcast >>= maybe (return ()) go
--  where
--    go = undefined

{-----------------------------------------------------------------------
    P2P monad
-----------------------------------------------------------------------}

newtype P2P a = P2P {
    runP2P :: ReaderT PeerSession PeerWire a
  } deriving (Functor, Applicative, Monad, MonadReader PeerSession, MonadIO)

withPeer :: SwarmSession -> PeerAddr -> P2P () -> IO ()
withPeer se addr p2p =
  withPeerSession se addr $ \(sock, pses) -> do
    runConduit sock (runReaderT (runP2P p2p) pses)
