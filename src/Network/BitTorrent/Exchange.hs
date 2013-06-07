-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE DoAndIfThenElse #-}
module Network.BitTorrent.Exchange (module PW) where

import Network.BitTorrent.Exchange.Selection as PW
import Network.BitTorrent.Exchange.Protocol as PW

{-

newtype P2P a = P2P {
    getP2P :: ReaderT PSession State PState (Conduit Message IO Message) a
  }

runP2P :: PConnection -> P2P a -> IO a
recvMessage :: P2P Message
sendMessage :: Message -> P2P ()
-}