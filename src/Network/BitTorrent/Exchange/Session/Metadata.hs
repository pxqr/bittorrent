{-# LANGUAGE TemplateHaskell #-}
module Network.BitTorrent.Exchange.Session.Metadata
       ( -- * Transfer state
         Status
       , nullStatus

         -- * State updates
       , Updates
       , runUpdates

         -- * Piece transfer control
       , scheduleBlock
       , resetPending
       , cancelPending
       , pushBlock
       ) where

import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Default
import Data.List as L
import Data.Tuple

import Data.BEncode as BE
import Data.Torrent as Torrent
import Network.BitTorrent.Address
import Network.BitTorrent.Exchange.Block   as Block
import Network.BitTorrent.Exchange.Message as Message hiding (Status)


-- | Current transfer status.
data Status = Status
  { _pending :: [(PeerAddr IP, PieceIx)]
  , _bucket  :: Bucket
  }

makeLenses ''Status

instance Default Status where
  def = error "default status"

-- | Create a new scheduler for infodict of the given size.
nullStatus :: Int -> Status
nullStatus ps = Status [] (Block.empty ps)

type Updates = ReaderT (PeerAddr IP) (State Status)

runUpdates :: MVar Status -> PeerAddr IP -> Updates a -> IO a
runUpdates v a m = modifyMVar v (return . swap . runState (runReaderT m a))

scheduleBlock :: Updates (Maybe PieceIx)
scheduleBlock = do
  addr <- ask
  bkt  <- use bucket
  case spans metadataPieceSize bkt of
    []              -> return Nothing
    ((off, _ ) : _) -> do
      let pix = off `div` metadataPieceSize
      pending %= ((addr, pix) :)
      return (Just pix)

cancelPending :: PieceIx -> Updates ()
cancelPending pix = pending %= L.filter ((pix ==) . snd)

resetPending :: Updates ()
resetPending = do
  addr <- ask
  pending %= L.filter ((addr ==) . fst)

parseInfoDict :: BS.ByteString -> InfoHash -> Result InfoDict
parseInfoDict chunk topic =
  case BE.decode chunk of
    Right (infodict @ InfoDict {..})
      | topic == idInfoHash -> return infodict
      |      otherwise      -> Left "broken infodict"
    Left err -> Left $ "unable to parse infodict " ++ err

-- todo use incremental parsing to avoid BS.concat call
pushBlock :: Torrent.Piece BS.ByteString -> InfoHash -> Updates (Maybe InfoDict)
pushBlock Torrent.Piece {..} topic = do
  addr <- ask
  p    <- use pending
  when ((addr, pieceIndex) `L.notElem` p) $ error "not requested"
  cancelPending pieceIndex

  bucket %= Block.insert (metadataPieceSize * pieceIndex) pieceData
  b <- use bucket
  case toPiece b of
    Nothing     -> return Nothing
    Just chunks ->
        case parseInfoDict (BL.toStrict chunks) topic of
          Right x -> do
              pending .= []
              return (Just x)
          Left  e -> do
              pending .= []
              bucket  .= Block.empty (Block.size b)
              return Nothing
