module Network.BitTorrent.Exchange.Manager
       ( Options (..)
       , Manager
       , Handler
       , newManager
       , closeManager
       ) where

import Control.Concurrent
import Control.Exception hiding (Handler)
import Control.Monad
import Data.Default
import Network.Socket

import Network.BitTorrent.Core


data Options = Options
  { optBacklog  :: Int
  , optPeerAddr :: PeerAddr IP
  } deriving (Show, Eq)

instance Default Options where
  def = Options
    { optBacklog  = maxListenQueue
    , optPeerAddr = def
    }

data Manager = Manager
  { listener :: !ThreadId
  }

type Handler = Socket -> PeerAddr IP -> IO ()

listenIncoming :: Options -> Handler -> IO ()
listenIncoming Options {..} handler = do
  bracket (socket AF_INET Stream defaultProtocol) close $ \ sock -> do
    bind sock (toSockAddr optPeerAddr)
    listen sock optBacklog
    forever $ do
      (conn, addr) <- accept sock
      case fromSockAddr addr of
        Nothing    -> return ()
        Just paddr -> do
          forkIO $ handler conn paddr
          return ()

newManager :: Options -> Handler -> IO Manager
newManager opts handler = do
  tid <- forkIO $ listenIncoming opts handler
  return (Manager tid)

closeManager :: Manager -> IO ()
closeManager Manager {..} = do
  killThread listener