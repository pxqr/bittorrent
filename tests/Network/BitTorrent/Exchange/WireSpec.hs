{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.BitTorrent.Exchange.WireSpec (spec) where
import Control.Applicative
import Control.Monad.Trans
import Data.Default
import Test.Hspec
import Test.QuickCheck

import Data.Torrent
import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.Exchange.Message
import Network.BitTorrent.Exchange.Wire

import Config
import Network.BitTorrent.Exchange.MessageSpec ()

nullSession :: InfoHash -> PeerId -> SessionLink ()
nullSession ih pid = SessionLink ih pid Nothing Nothing ()

instance Arbitrary Options where
  arbitrary = return def

instance Arbitrary ConnectionPrefs where
  arbitrary = ConnectionPrefs <$> arbitrary <*> pure def
                              <*> arbitrary <*> arbitrary

withWire :: ConnectionPrefs -> Wire () () -> IO ()
withWire prefs wire =
  withRemote $ \ ClientOpts {..} -> do
    pid <- genPeerId
    t   <- getTestTorrent
    let ih  = idInfoHash (tInfoDict t)
    let cfg = ConnectionConfig prefs (nullSession ih pid) (wire)
    let addr = PeerAddr Nothing "127.0.0.1" peerPort
    connectWire addr cfg

spec :: Spec
spec = do
  describe "connectWire" $ do
    it "can establish connection with all possible preferences" $
      property $ \ prefs -> do
        withWire prefs (return ())

    it "must not connect with invalid topic" $ do
      pending

  describe "acceptWire" $ do
    it "" $ do
      pending

  describe "messaging" $ do
    it "first message is bitfield" $ do
      withWire def $ do
        msg <- recvMessage
        let isBitfield (Available (Bitfield _)) = True
            isBitfield  _                       = False
        liftIO $ msg `shouldSatisfy` isBitfield
