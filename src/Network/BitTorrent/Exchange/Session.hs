{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.BitTorrent.Exchange.Session
       ( Session
       , newSession
       , closeSession

       , Network.BitTorrent.Exchange.Session.insert
       ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.IORef
import Data.Map
import Data.Ord
import Data.Typeable
import Text.PrettyPrint

import Data.Torrent.Bitfield
import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Assembler
import Network.BitTorrent.Exchange.Block
import Network.BitTorrent.Exchange.Message
import Network.BitTorrent.Exchange.Status
import Network.BitTorrent.Exchange.Wire
import System.Torrent.Storage


data ExchangeError
  = InvalidPieceIx PieceIx
  | InvalidBlock   BlockIx
  | CorruptedPiece PieceIx

data Session = Session
  { peerId      :: PeerId
  , bitfield    :: Bitfield
  , assembler   :: Assembler
  , storage     :: Storage
  , unchoked    :: [PeerAddr IP]
  , handler     :: Exchange ()
  , connections :: Map (PeerAddr IP) Connection
  }

newSession :: PeerAddr IP -> Storage -> Bitfield -> IO Session
newSession addr st bf = do
  return Session
    { peerId      = undefined
    , bitfield    = undefined
    , assembler   = undefined
    , storage     = undefined
    , unchoked    = undefined
    , handler     = undefined
    , connections = undefined
    }

closeSession :: Session -> IO ()
closeSession = undefined

insert :: PeerAddr IP -> {- Maybe Socket -> -} Session -> IO ()
insert addr ses @ Session {..} = do
  undefined
--  forkIO $ connectWire hs addr caps (runStateT ses handler)

delete :: PeerAddr IP -> Session -> IO ()
delete = undefined

deleteAll :: Session -> IO ()
deleteAll = undefined

{-----------------------------------------------------------------------
--  Event loop
-----------------------------------------------------------------------}

type Exchange = StateT Session (ReaderT Connection IO)

--runExchange :: Exchange () -> [PeerAddr] -> IO ()
--runExchange exchange peers = do
--  forM_ peers $ \ peer -> do
--    forkIO $ runReaderT (runStateT exchange session )

data Event = NewMessage (PeerAddr IP) Message
           | Timeout -- for scheduling

awaitEvent :: Exchange Event
awaitEvent = undefined

yieldEvent :: Exchange Event
yieldEvent = undefined
