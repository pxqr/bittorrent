{-# LANGUAGE TypeFamilies #-}
module Network.BitTorrent.Internal.Types
       ( EventSource (..)
       ) where

import Control.Concurrent.Chan.Split

class EventSource source  where
  data Event source
  listen :: source -> IO (ReceivePort (Event source))
