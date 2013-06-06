-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
module Network.BitTorrent.Peer.Status
       ( PeerStatus(..)
       , setChoking, setInterested
       , initPeerStatus

       , SessionStatus(..)
       , initSessionStatus
       , setClientStatus, setPeerStatus
       , canUpload, canDownload

       -- * Defaults
       , defaultUnchokeSlots
       ) where

data PeerStatus = PeerStatus {
    psChoking    :: Bool
  , psInterested :: Bool
  }

-- | Any session between peers starts as choking and not interested.
initPeerStatus :: PeerStatus
initPeerStatus = PeerStatus True False

setChoking :: Bool -> PeerStatus -> PeerStatus
setChoking b ps = ps { psChoking = b }

setInterested :: Bool -> PeerStatus -> PeerStatus
setInterested b ps = ps { psInterested = b }



data SessionStatus = SessionStatus {
    seClientStatus :: PeerStatus
  , sePeerStatus   :: PeerStatus
  }

initSessionStatus :: SessionStatus
initSessionStatus = SessionStatus initPeerStatus initPeerStatus

setClientStatus :: (PeerStatus -> PeerStatus) -> SessionStatus -> SessionStatus
setClientStatus f ss = ss { seClientStatus = f (seClientStatus ss) }

setPeerStatus :: (PeerStatus -> PeerStatus) -> SessionStatus -> SessionStatus
setPeerStatus f ss = ss { sePeerStatus = f (sePeerStatus ss) }

-- | Can the /client/ to upload to the /peer/?
canUpload :: SessionStatus -> Bool
canUpload SessionStatus { seClientStatus = client, sePeerStatus = peer}  =
  psInterested peer && not (psChoking client)

-- | Can the /client/ download from the /peer/?
canDownload :: SessionStatus -> Bool
canDownload SessionStatus { seClientStatus = client, sePeerStatus = peer }  =
  psInterested client && not (psChoking peer)

-- | Indicates have many peers are allowed to download from the client.
defaultUnchokeSlots :: Int
defaultUnchokeSlots = 4