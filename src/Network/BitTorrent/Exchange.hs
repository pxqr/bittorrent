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
      return msg

signalMessage :: Message -> PeerSession -> PeerWire ()
signalMessage msg se = do
  C.yield msg
  liftIO $ updateOutcoming se


getPieceCount :: PeerSession -> IO PieceCount
getPieceCount = undefined

nextEvent :: PeerSession -> PeerWire Event
nextEvent se @ PeerSession {..} = waitMessage se >>= diff
  where
    -- diff finds difference between
--    diff KeepAlive = nextEvent se
    diff msg = do
      liftIO $ print (ppMessage msg)
      nextEvent se

    handleMessage Choke     = do
      SessionStatus {..} <- liftIO $ readIORef peerSessionStatus
      if psChoking  sePeerStatus
        then nextEvent se
        else undefined

    handleMessage Unchoke   = undefined
    --return $ Available BF.difference

    handleMessage Interested = return undefined
    handleMessage NotInterested = return undefined
    handleMessage (Have ix) = do
      pc <- liftIO $ getPieceCount se
      haveMessage $ have ix (haveNone pc) -- TODO singleton

    handleMessage (Bitfield bf) = undefined
    handleMessage (Request  bix) = do
      undefined

    handleMessage msg @ (Piece    blk) = undefined
    handleMessage msg @ (Port     _)
      = checkExtension msg ExtDHT $ do
      undefined

    handleMessage msg @ HaveAll
      = checkExtension msg ExtFast $ do
          pc <- liftIO $ getPieceCount se
          haveMessage (haveAll pc)

    handleMessage msg @ HaveNone
      = checkExtension msg ExtFast $ do
        pc <- liftIO $ getPieceCount se
        haveMessage (haveNone pc)

    handleMessage msg @ (SuggestPiece ix)
          = checkExtension msg ExtFast $ do
            undefined

    handleMessage msg @ (RejectRequest ix)
          = checkExtension msg ExtFast $ do
            undefined

    handleMessage msg @ (AllowedFast pix)
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

instance MonadState Bitfield P2P where

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
