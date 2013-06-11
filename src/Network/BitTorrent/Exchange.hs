-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Exchange
       ( P2P, withPeer
       , awaitEvent, signalEvent
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
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

import Network


import Network.BitTorrent.Internal
import Network.BitTorrent.Extension
import Network.BitTorrent.Peer
import Network.BitTorrent.Exchange.Protocol
import Data.Bitfield as BF
import Data.Torrent


data Event = Available Bitfield
           | Want
           | Block
             deriving Show

{-----------------------------------------------------------------------
    Peer wire
-----------------------------------------------------------------------}

type PeerWire = ConduitM Message Message IO

waitMessage :: PeerSession -> PeerWire Message
waitMessage se = do
  mmsg <- await
  case mmsg of
    Nothing -> waitMessage se
    Just msg -> do
      liftIO $ updateIncoming se
      liftIO $ print msg
      return msg

signalMessage :: PeerSession -> Message -> PeerWire ()
signalMessage se msg = do
  C.yield msg
  liftIO $ updateOutcoming se


getPieceCount :: PeerSession -> IO PieceCount
getPieceCount = undefined

canOffer :: PeerSession -> PeerWire Bitfield
canOffer PeerSession {..} = liftIO $ do
  pbf <- readIORef $ peerBitfield
  cbf <- readIORef $ clientBitfield $ swarmSession
  return $ BF.difference pbf cbf

revise :: PeerSession -> PeerWire ()
revise se @ PeerSession {..} = do
  isInteresting <- (not . BF.null) <$> canOffer se
  SessionStatus {..} <- liftIO $ readIORef peerSessionStatus

  when (isInteresting /= _interested seClientStatus) $
    signalMessage se $ if isInteresting then Interested else NotInterested


nextEvent :: PeerSession -> PeerWire Event
nextEvent se @ PeerSession {..} = waitMessage se >>= go
  where
    go KeepAlive = nextEvent se
    go Choke     = do
      SessionStatus {..} <- liftIO $ readIORef peerSessionStatus
      if _choking  sePeerStatus
        then nextEvent se
        else undefined

    go Unchoke   = do
      SessionStatus {..} <- liftIO $ readIORef peerSessionStatus
      if not (_choking sePeerStatus)
        then nextEvent se
        else if undefined
             then undefined
             else undefined
    --return $ Available BF.difference

    go Interested = return undefined
    go NotInterested = return undefined

    go (Have ix) = do
      pc <- liftIO $ getPieceCount se
      haveMessage $ have ix (haveNone pc) -- TODO singleton

    go (Bitfield bf) = undefined
    go (Request  bix) = do
      undefined

    go msg @ (Piece    blk) = undefined
    go msg @ (Port     _)
      = checkExtension msg ExtDHT $ do
      undefined

    go msg @ HaveAll
      = checkExtension msg ExtFast $ do
          pc <- liftIO $ getPieceCount se
          haveMessage (haveAll pc)

    go msg @ HaveNone
      = checkExtension msg ExtFast $ do
        pc <- liftIO $ getPieceCount se
        haveMessage (haveNone pc)

    go msg @ (SuggestPiece ix)
          = checkExtension msg ExtFast $ do
            undefined

    go msg @ (RejectRequest ix)
          = checkExtension msg ExtFast $ do
            undefined

    go msg @ (AllowedFast pix)
          = checkExtension msg ExtFast $ do
            undefined

    haveMessage bf = do
      cbf <- liftIO $ readIORef $ clientBitfield swarmSession
      if undefined -- ix `member` bf
        then nextEvent se
        else undefined  -- return $ Available diff

    checkExtension msg requredExtension action
      | requredExtension `elem` enabledExtensions = action
      | otherwise = liftIO $ throwIO $ userError errorMsg
      where
        errorMsg = show (ppExtension requredExtension)
                   ++  "not enabled, but peer sent"
                   ++ show (ppMessage msg)

{-----------------------------------------------------------------------
    P2P monad
-----------------------------------------------------------------------}

newtype P2P a = P2P {
    runP2P :: ReaderT PeerSession PeerWire a
  } deriving (Monad, MonadReader PeerSession, MonadIO)

instance MonadState SessionStatus P2P where
  get   = asks peerSessionStatus >>= liftIO . readIORef
  put x = asks peerSessionStatus >>= liftIO . (`writeIORef` x)


runConduit :: Socket -> Conduit Message IO Message -> IO ()
runConduit sock p2p =
  sourceSocket sock   $=
    conduitGet S.get  $=
      forever p2p     $=
    conduitPut S.put  $$
  sinkSocket sock

withPeer :: SwarmSession -> PeerAddr -> P2P () -> IO ()
withPeer se addr p2p =
  withPeerSession se addr $ \(sock, pses) -> do
    runConduit sock (runReaderT (runP2P p2p) pses)


awaitEvent :: P2P Event
awaitEvent = P2P (ReaderT nextEvent)

signalEvent  :: Event -> P2P ()
signalEvent = undefined
