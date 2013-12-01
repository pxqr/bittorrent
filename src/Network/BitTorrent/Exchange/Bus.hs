module Network.BitTorrent.Exchange.Bus ( ) where

type PeerWire = ConduitM Message Message IO

runPeerWire :: Socket -> PeerWire () -> IO ()
runPeerWire sock action =
  sourceSocket sock     $=
    S.conduitGet S.get  $=
--    B.conduitDecode     $=
      action            $=
    S.conduitPut S.put  $$
--    B.conduitEncode     $$
  sinkSocket sock

awaitMessage :: P2P Message
awaitMessage = P2P $ ReaderT $ const $ {-# SCC awaitMessage #-} go
  where
    go = await >>= maybe (monadThrow PeerDisconnected) return
{-# INLINE awaitMessage #-}

yieldMessage :: Message -> P2P ()
yieldMessage msg = P2P $ ReaderT $ const $ {-# SCC yieldMessage #-} do
  C.yield msg
{-# INLINE yieldMessage #-}

-- TODO send vectored
flushPending :: P2P ()
flushPending = {-# SCC flushPending #-} do
  session <- ask
  queue   <- liftIO (getPending session)
  mapM_ yieldMessage queue

{-----------------------------------------------------------------------
    P2P monad
-----------------------------------------------------------------------}

filterMeaninless :: P2P Message Message
filterMeaninless = undefined

-- |
--   Exceptions:
--
--     * SessionException: is visible only within one peer
--     session. Use this exception to terminate P2P session, but not
--     the swarm session.
--
newtype P2P a = P2P {
    unP2P :: ReaderT PeerSession PeerWire a
  } deriving ( Functor, Applicative, Monad
             , MonadIO, MonadThrow, MonadActive
             , MonadReader PeerSession
             )

instance MonadState SessionState P2P where
  get    = asks sessionState >>= liftIO . readIORef
  {-# INLINE get #-}
  put !s = asks sessionState >>= \ref -> liftIO $ writeIORef ref s
  {-# INLINE put #-}

runP2P :: (Socket, PeerSession) -> P2P () -> IO ()
runP2P (sock, ses) action =
  handle isIOException $
    runPeerWire sock (runReaderT (unP2P action) ses)
  where
    isIOException :: IOException -> IO ()
    isIOException _ = return ()
