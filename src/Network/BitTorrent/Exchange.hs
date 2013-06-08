-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE DoAndIfThenElse #-}
module Network.BitTorrent.Exchange
       (
         -- * Session
         PeerSession, newLeacher, newSeeder
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Data.Function
import Data.Ord
import Data.Set as S

import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize

import Network.BitTorrent.Exchange.Selection as PW
import Network.BitTorrent.Exchange.Protocol as PW

import Network.BitTorrent.Internal
import Network.BitTorrent.Extension
import Network.BitTorrent.Peer
import Data.Bitfield as BF
import Data.Torrent

{-----------------------------------------------------------------------
    P2P monad
-----------------------------------------------------------------------}

{-
type P2P = Reader PeerSession (ConduitM Message Message IO)

conduit :: Socket -> P2P a -> IO a
conduit sock p2p =
  sourceSocket sock   $=
    conduitGet get    $=
      messageLoop p2p $=
    conduitPut put    $$
  sinkSocket sock

messageLoop :: P2P () -> P2P ()
messageLoop = undefined

runP2P :: SSession -> PeerAddr -> P2P a -> IO a
runP2P se addr p2p = withPeer se addr $ conduit messageLoop

data Event = Available
           | Want
           | Block

{-
waitForEvent :: P2P Event
signalEvent  :: Event -> P2P ()
-}
-}