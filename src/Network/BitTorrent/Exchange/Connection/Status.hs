-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Each P2P connection endpoint should keep track status of both
--   sides.
--
{-# LANGUAGE TemplateHaskell   #-}
module Network.BitTorrent.Exchange.Connection.Status
       ( -- * Peer status
         PeerStatus(..)
       , choking
       , interested

         -- ** Query
       , updateStatus
       , statusUpdates

         -- * Connection status
       , ConnectionStatus(..)
       , clientStatus
       , remoteStatus

         -- ** Query
       , canUpload
       , canDownload

         -- * Extra
       , defaultUnchokeSlots
       , defaultRechokeInterval
       ) where

import Control.Lens
import Data.Aeson.TH
import Data.Default
import Data.Maybe
import Data.Monoid
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class

import Data.Torrent.JSON
import Network.BitTorrent.Exchange.Message


{-----------------------------------------------------------------------
--  Peer status
-----------------------------------------------------------------------}

-- | Connections contain two bits of state on either end: choked or
-- not, and interested or not.
data PeerStatus = PeerStatus
  { -- | Choking is a notification that no data will be sent until
    -- unchoking happens.
    _choking    :: !Bool

    -- |
  , _interested :: !Bool
  } deriving (Show, Eq, Ord)

$(makeLenses ''PeerStatus)
$(deriveJSON omitLensPrefix ''PeerStatus)

instance Pretty PeerStatus where
  pretty PeerStatus {..} =
    pretty (Choking _choking) <+> "and" <+> pretty (Interested _interested)

-- | Connections start out choked and not interested.
instance Default PeerStatus where
  def = PeerStatus True False

instance Monoid PeerStatus where
  mempty      = def
  mappend a b = PeerStatus
    { _choking    = _choking    a && _choking    b
    , _interested = _interested a || _interested b
    }

-- | Can be used to update remote peer status using incoming 'Status'
-- message.
updateStatus :: StatusUpdate -> PeerStatus -> PeerStatus
updateStatus (Choking    b) = choking    .~ b
updateStatus (Interested b) = interested .~ b

-- | Can be used to generate outcoming messages.
statusUpdates :: PeerStatus -> PeerStatus -> [StatusUpdate]
statusUpdates a b = catMaybes $
  [ if _choking    a == _choking    b then Nothing
    else Just $ Choking    $ _choking    b
  , if _interested a == _interested b then Nothing
    else Just $ Interested $ _interested b
  ]

{-----------------------------------------------------------------------
--  Connection status
-----------------------------------------------------------------------}

-- | Status of the both endpoints.
data ConnectionStatus = ConnectionStatus
  { _clientStatus :: !PeerStatus
  , _remoteStatus :: !PeerStatus
  } deriving (Show, Eq)

$(makeLenses ''ConnectionStatus)
$(deriveJSON omitRecordPrefix ''ConnectionStatus)

instance Pretty ConnectionStatus where
  pretty ConnectionStatus {..} =
    "this  " <+> pretty _clientStatus $$
    "remote" <+> pretty _remoteStatus

-- | Connections start out choked and not interested.
instance Default ConnectionStatus where
  def = ConnectionStatus def def

-- | Can the client transfer to the remote peer?
canUpload :: ConnectionStatus -> Bool
canUpload ConnectionStatus {..}
  = _interested _remoteStatus && not (_choking _clientStatus)

-- | Can the client transfer from the remote peer?
canDownload :: ConnectionStatus -> Bool
canDownload ConnectionStatus {..}
  = _interested _clientStatus && not (_choking _remoteStatus)

-- | Indicates how many peers are allowed to download from the client
-- by default.
defaultUnchokeSlots :: Int
defaultUnchokeSlots = 4

-- |
defaultRechokeInterval :: Int
defaultRechokeInterval = 10 * 1000 * 1000