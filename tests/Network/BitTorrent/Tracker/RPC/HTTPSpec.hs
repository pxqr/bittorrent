{-# LANGUAGE RecordWildCards #-}
module Network.BitTorrent.Tracker.RPC.HTTPSpec (spec, trackerURIs) where

import Control.Applicative
import Control.Monad
import Data.Default
import Data.List as L
import Data.Maybe
import Network.URI
import Test.Hspec

import Data.Torrent.Progress
import Network.BitTorrent.Tracker.Message as Message
import Network.BitTorrent.Tracker.RPC.HTTP

-- TODO add a good working tracker!
trackerURIs :: [URI]
trackerURIs = fmap (fromJust . parseURI)
  [ "http://tracker.openbittorrent.com:80/announce"
  , "http://tracker.publicbt.com:80/announce"
  ]

validateInfo :: AnnounceQuery -> AnnounceInfo -> Expectation
validateInfo _ Message.Failure {..} = error "validateInfo: failure"
validateInfo AnnounceQuery {..}  AnnounceInfo {..} = do
  case respComplete <|> respIncomplete of
    Nothing -> return ()
    Just n  -> n  `shouldBe` L.length (getPeerList respPeers)

spec :: Spec
spec = do
  forM_ trackerURIs $ \ uri ->
    context (show uri) $ do
      describe "announce" $ do
        it "have valid response" $ do
          withManager def $ \ mgr -> do
--            q    <- arbitrarySample
            let q = AnnounceQuery def "-HS0003-203534.37420" 6000
                    (Progress 0 0 0) Nothing Nothing (Just Started)
            info <- announce mgr uri q
            validateInfo q info

      describe "scrape" $ do
        it "have valid response" $ do
          withManager def $ \ mgr -> do
            xs <- scrape mgr uri [def]
            L.length xs `shouldSatisfy` (>= 1)
