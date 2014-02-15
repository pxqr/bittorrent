{-# LANGUAGE TemplateHaskell #-}
module Network.BitTorrent.Exchange.Session.Metadata
       ( -- * Metadata transfer state
         Status
       , nullStatus

         -- * Metadata updates
       , Updates
       , runUpdates

         -- * Metadata piece control
       , scheduleBlock
       , resetPending
       , cancelPending
       , pushBlock
       ) where

import Control.Concurrent
import Control.Lens
import Control.Monad.State
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.List as L

import Data.BEncode as BE
import Data.Torrent
import Data.Torrent.InfoHash
import Data.Torrent.Piece as Torrent
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Block   as Block
import Network.BitTorrent.Exchange.Message as Message hiding (Status)


data Status = Status
  { _pending :: [(PeerAddr IP, PieceIx)]
  , _bucket  :: Bucket
  }

makeLenses ''Status

nullStatus :: PieceSize -> Status
nullStatus ps = Status [] (Block.empty ps)

type Updates a = State Status a

runUpdates :: MVar Status -> Updates a -> IO a
runUpdates v m = undefined

scheduleBlock :: PeerAddr IP -> Updates (Maybe PieceIx)
scheduleBlock addr = do
  bkt <- use bucket
  case spans metadataPieceSize bkt of
    []              -> return Nothing
    ((off, _ ) : _) -> do
      let pix = undefined
      pending %= ((addr, pix) :)
      return (Just pix)

cancelPending :: PieceIx -> Updates ()
cancelPending pix = pending %= L.filter ((pix ==) . snd)

resetPending :: PeerAddr IP -> Updates ()
resetPending addr = pending %= L.filter ((addr ==) . fst)

parseInfoDict :: BS.ByteString -> InfoHash -> Result InfoDict
parseInfoDict chunk topic =
  case BE.decode chunk of
    Right (infodict @ InfoDict {..})
      | topic == idInfoHash -> return infodict
      |      otherwise      -> Left "broken infodict"
    Left err -> Left $ "unable to parse infodict " ++ err

-- todo use incremental parsing to avoid BS.concat call
pushBlock :: PeerAddr IP -> Torrent.Piece BS.ByteString -> InfoHash
          -> Updates (Maybe InfoDict)
pushBlock addr Torrent.Piece {..} topic = do
  p <- use pending
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
