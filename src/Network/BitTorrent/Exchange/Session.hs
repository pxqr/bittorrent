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


data ExchangeError
  = InvalidPieceIx PieceIx
  | InvalidBlock   BlockIx
  | CorruptedPiece PieceIx

data Session = Session
  { storage   :: Storage
  , bitfield  :: Bitfield
  , assembler :: Assembler
  , peerId    :: PeerId
  }

type Exchange = StateT Session (ReaderT Connection IO)

--runExchange :: Exchange () -> [PeerAddr] -> IO ()
--runExchange exchange peers = do
--  forM_ peers $ \ peer -> do
--    forkIO $ runReaderT (runStateT exchange session )

awaitEvent :: Exchange Event
awaitEvent = undefined

yieldEvent :: Exchange Event
yieldEvent = undefined
