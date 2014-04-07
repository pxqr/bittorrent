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

import Data.Torrent
import Network.BitTorrent.Address
import Network.BitTorrent.Exchange.Connection hiding (Options)
import Network.BitTorrent.Exchange.Session


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

type Handler = InfoHash -> IO Session

handleNewConn :: Socket -> PeerAddr IP -> Handler -> IO ()
handleNewConn sock addr handler = do
  conn <- newPendingConnection sock addr
  ses  <- handler (pendingTopic conn) `onException` closePending conn
  establish conn ses

listenIncoming :: Options -> Handler -> IO ()
listenIncoming Options {..} handler = do
  bracket (socket AF_INET Stream defaultProtocol) close $ \ sock -> do
    bind sock (toSockAddr optPeerAddr)
    listen sock optBacklog
    forever $ do
      (conn, sockAddr) <- accept sock
      case fromSockAddr sockAddr of
        Nothing   -> return ()
        Just addr -> void $ forkIO $ handleNewConn sock addr handler

newManager :: Options -> Handler -> IO Manager
newManager opts handler = do
  tid <- forkIO $ listenIncoming opts handler
  return (Manager tid)

closeManager :: Manager -> IO ()
closeManager Manager {..} = do
  killThread listener