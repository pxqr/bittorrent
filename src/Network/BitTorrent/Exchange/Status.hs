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
module Network.BitTorrent.Exchange.Status
       ( -- * Peer status
         PeerStatus(..)
       , choking
       , interested

         -- ** Query
       , updateStatus
       , statusUpdates

         -- * Session status
       , SessionStatus(..)
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
import Data.List as L
import Data.Maybe
import Data.Monoid
import Text.PrettyPrint as PP hiding ((<>))
import Text.PrettyPrint.Class

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
$(deriveJSON defaultOptions { fieldLabelModifier = L.tail } ''PeerStatus)

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
--  Session status
-----------------------------------------------------------------------}

-- | Status of the both endpoints.
data SessionStatus = SessionStatus
  { _clientStatus :: !PeerStatus
  , _remoteStatus :: !PeerStatus
  } deriving (Show, Eq)

$(makeLenses ''SessionStatus)
$(deriveJSON defaultOptions { fieldLabelModifier = L.tail } ''SessionStatus)

instance Pretty SessionStatus where
  pretty SessionStatus {..} =
    "this  " <+> pretty _clientStatus $$
    "remote" <+> pretty _remoteStatus

-- | Connections start out choked and not interested.
instance Default SessionStatus where
  def = SessionStatus def def

-- | Can the client transfer to the remote peer?
canUpload :: SessionStatus -> Bool
canUpload SessionStatus {..}
  = _interested _remoteStatus && not (_choking _clientStatus)

-- | Can the client transfer from the remote peer?
canDownload :: SessionStatus -> Bool
canDownload SessionStatus {..}
  = _interested _clientStatus && not (_choking _remoteStatus)

-- | Indicates how many peers are allowed to download from the client
-- by default.
defaultUnchokeSlots :: Int
defaultUnchokeSlots = 4

-- |
defaultRechokeInterval :: Int
defaultRechokeInterval = 10 * 1000 * 1000