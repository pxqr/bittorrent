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

import Network.BitTorrent.Exchange.Selection
import Network.BitTorrent.Exchange.Protocol

import Network.BitTorrent.Internal
import Network.BitTorrent.Extension
import Network.BitTorrent.Peer
import Data.Bitfield as BF
import Data.Torrent

{-----------------------------------------------------------------------
    P2P monad
-----------------------------------------------------------------------}

type PeerWire = ConduitM Message Message IO

waitMessage :: PeerWire Message
waitMessage = await >>= maybe waitMessage return

signalMessage :: Message -> PeerWire ()
signalMessage = C.yield

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

data Event = Available Bitfield
           | Want
           | Block



waitForEvent :: P2P Event
waitForEvent = P2P (ReaderT nextEvent)
  where
    nextEvent se @ PeerSession {..} = waitMessage >>= diff
      where
        -- diff finds difference between
        diff KeepAlive = do
          signalMessage KeepAlive
          nextEvent se

        handleMessage Choke     = do
          SessionStatus {..} <- liftIO $ readIORef peerSessionStatus
          if psChoking  sePeerStatus
            then nextEvent se
            else undefined

        handleMessage Unchoke   = return $ Available BF.difference

        handleMessage Interested = return undefined
        handleMessage NotInterested = return undefined

        handleMessage (Have ix) = do
          pc <- liftIO $ getPieceCount se
          haveMessage $ have ix (haveNone pc) -- TODO singleton

        handleMessage (Bitfield bf) = undefined
        handleMessage (Request  bix) = do
          undefined

        handleMessage (Piece    blk) = undefined
        handleMessage (Port     _)
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
            else return $ Available diff

        checkExtension msg requredExtension action
            | requredExtension `elem` enabledExtensions = action
            | otherwise = liftIO $ throwIO $ userError errorMsg
          where
            errorMsg = show (ppExtension requredExtension)
                    ++  "not enabled, but peer sent"
                    ++ show (ppMessage msg)



getPieceCount :: PeerSession -> IO PieceCount
getPieceCount = undefined

signalEvent  :: Event -> P2P ()
signalEvent = undefined
