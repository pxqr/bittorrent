-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
--   This module provides high level API for peer -> tracker
--   communication. Tracker is used to discover other peers in the
--   network using torrent info hash.
--
{-# LANGUAGE TemplateHaskell   #-}
module Network.BitTorrent.Tracker
       ( -- * Connection
         TConnection(..)
       , tconnection

         -- * Session
       , TSession
       , tracker

         -- * Re-export
       , defaultPorts
       , ScrapeInfo
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.BoundedChan as BC
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.List as L
import Data.IORef
import Data.Text as T
import Network
import Network.URI

import Data.Torrent
import Network.BitTorrent.Peer
import Network.BitTorrent.Tracker.Protocol as Tracker
import Network.BitTorrent.Tracker.HTTP
import Network.BitTorrent.Tracker.UDP

{-----------------------------------------------------------------------
    Generalized Tracker instance — UDP + HTTP
-----------------------------------------------------------------------}

data BitTracker = HTTPTr HTTPTracker
                | UDPTr UDPTracker

instance Tracker BitTracker where
  connect uri @ URI {..}
    | uriScheme == "udp:"  = UDPTr  <$> connect uri
    | uriScheme == "http:" = HTTPTr <$> connect uri
    |       otherwise      = throwIO $ userError msg
    where
      msg = "unknown tracker protocol scheme: " ++ show uriScheme

  announce (HTTPTr t) = Tracker.announce t
  announce (UDPTr  t) = Tracker.announce t

  scrape (HTTPTr t) = scrape t
  scrape (UDPTr  t) = scrape t

{-----------------------------------------------------------------------
    Tracker connection
-----------------------------------------------------------------------}

-- | 'TConnection' (shorthand for Tracker session) combines tracker
-- request fields neccessary for tracker, torrent and client
-- identification.
--
--   This data is considered as static within one session.
--
data TConnection = TConnection {
    tconnAnnounce :: URI
                     -- ^ Announce URL.
  , tconnInfoHash :: InfoHash
                     -- ^ Hash of info part of current .torrent file.
  , tconnPeerId   :: PeerId
                     -- ^ Client peer ID.
  , tconnPort     :: PortNumber
                     -- ^ The port number the client is listenning on.
  } deriving Show

-- TODO tconnection :: SwarmSession -> TConnection
tconnection :: Torrent -> PeerId -> PortNumber -> TConnection
tconnection t = TConnection (tAnnounce t) (tInfoHash t)

-- | used to avoid boilerplate; do NOT export me
genericReq :: TConnection -> Progress -> AnnounceQuery
genericReq ses pr =   AnnounceQuery {
    reqInfoHash   = tconnInfoHash ses
  , reqPeerId     = tconnPeerId   ses
  , reqPort       = tconnPort     ses

  , reqProgress   = pr

  , reqIP         = Nothing
  , reqNumWant    = Nothing
  , reqEvent      = Nothing
  }

-- | The first request to the tracker that should be created is
--   'startedReq'. It includes necessary 'Started' event field.
--
startedReq :: TConnection -> Progress -> AnnounceQuery
startedReq ses pr = (genericReq ses pr)
  { reqNumWant    = Just defaultNumWant
  , reqEvent      = Just Started
  }

-- | Regular request must be sent to keep track new peers and
--   notify tracker about current state of the client
--   so new peers could connect to the client.
--
regularReq :: Int -> TConnection -> Progress -> AnnounceQuery
regularReq numWant ses pr = (genericReq ses pr)
  { reqNumWant    = Just numWant
  , reqEvent      = Nothing
  }

-- | Must be sent to the tracker if the client is shutting down
-- gracefully.
--
stoppedReq :: TConnection -> Progress -> AnnounceQuery
stoppedReq ses pr = (genericReq ses pr)
  { reqNumWant    = Nothing
  , reqEvent      = Just Stopped
  }

-- | Must be sent to the tracker when the download completes.
-- However, must not be sent if the download was already 100%
-- complete.
--
completedReq :: TConnection -> Progress -> AnnounceQuery
completedReq ses pr = (genericReq ses pr)
  { reqNumWant    = Nothing
  , reqEvent      = Just Completed
  }

{-----------------------------------------------------------------------
    Tracker session
-----------------------------------------------------------------------}

{-  Why use BoundedChan?

Because most times we need just a list of peer at the start and all
the rest time we will take little by little. On the other hand tracker
will give us some constant count of peers and channel will grow with
time. To avoid space leaks and long lists of peers (which we don't
need) we use bounded chaan.

   Chan size.

Should be at least (count_of_workers * 2) to accumulate long enough
peer list.

  Order of peers in chan.

Old peers in head, new ones in tail. Old peers should be used in the
first place because by statistics they are most likely will present in
network a long time than a new.

-}

type TimeInterval = Int

waitInterval :: TSession -> IO ()
waitInterval TSession {..} = do
    delay <- readIORef seInterval
    threadDelay (delay * sec)
  where
    sec = 1000 * 1000 :: Int

data TSession = TSession
  { seConnection :: !TConnection
  , seTracker    :: !BitTracker
  , seProgress   :: !(TVar Progress)
  , sePeers      :: !(BoundedChan PeerAddr)
  , seInterval   :: {-# UNPACK #-} !(IORef TimeInterval)
  }

openSession :: BoundedChan PeerAddr
            -> TVar Progress
            -> TConnection -> IO TSession
openSession chan progress conn @ TConnection {..} = do
  trac <- Tracker.connect tconnAnnounce
  pr   <- readTVarIO progress
  resp <- Tracker.announce trac $ startedReq conn pr
  print resp
  case resp of
    Failure e -> throwIO $ userError $ T.unpack e
    AnnounceInfo {..} -> do
      -- TODO make use of rest AnnounceInfo fields
      BC.writeList2Chan chan respPeers
      TSession conn trac progress chan
        <$> newIORef respInterval

closeSession :: TSession -> IO ()
closeSession TSession {..} = do
  pr <- readTVarIO seProgress
  _  <- Tracker.announce seTracker (stoppedReq seConnection pr)
  return ()

withSession :: BoundedChan PeerAddr
            -> TVar Progress
            -> TConnection -> (TSession -> IO a) -> IO a
withSession chan prog conn
  = bracket (openSession chan prog conn) closeSession

askPeers :: TSession -> IO ()
askPeers se @ TSession {..} = forever $ do
  waitInterval se
  pr   <- readTVarIO seProgress
  resp <- tryJust isIOException $ do
    let req = regularReq defaultNumWant seConnection pr
    Tracker.announce seTracker req
  print resp
  case resp of
    Left _ -> return ()
    Right (Failure e)         -> throwIO $ userError $ T.unpack e
    Right (AnnounceInfo {..}) -> do
      writeIORef seInterval respInterval

      -- we rely on the fact that union on lists is not
      -- commutative: this implements the heuristic "old peers
      -- in head"
      old <- BC.getChanContents sePeers
      let combined = L.union old respPeers
      BC.writeList2Chan sePeers combined
 where
    isIOException :: IOException -> Maybe IOException
    isIOException = return

tracker :: BoundedChan PeerAddr -> TVar Progress -> TConnection -> IO ()
tracker chan prog conn = withSession chan prog conn askPeers