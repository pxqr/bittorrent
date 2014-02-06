module Network.BitTorrent.Tracker.ListSpec (spec) where
import Control.Exception
import Data.Default
import Data.Foldable as F
import Data.List as L
import Data.Maybe
import Network.URI
import Test.Hspec

import Data.Torrent
import Network.BitTorrent.Tracker.List
import Network.BitTorrent.Tracker.RPC


uris :: [URI]
uris = fmap (fromJust . parseURI . renderURI) [1..10 :: Int]
  where
    renderURI n = "http://" ++ show n ++ ".org"

list :: TrackerList URI
list = fromJust $ trackerList def { tAnnounceList = Just [uris] }

spec :: Spec
spec = do
  describe "TrackerList" $ do
    it "trackerList is not empty" $ do
      pending

    it "shuffleTiers (may fail with very small probability)" $ do
      list' <- shuffleTiers list
      list' `shouldSatisfy` (/= list)

    it "traverseAll" $ do
      xs <- traverseAll (\ uri -> if uri == L.last uris
               then throwIO (GenericException "")
               else return uri { uriScheme = "udp://" }) list
      let udps = F.sum $ fmap (fromEnum . ("udp://" ==) . uriScheme) xs
      udps `shouldBe` pred (L.length uris)

    it "traverseTiers" $ do
      xs' <- traverseTiers (\ uri -> if uri == L.last uris then return uri
                              else throwIO (GenericException "")) list

      return ()
