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

list :: TrackerList ()
list = trackerList def { tAnnounceList = Just [uris] }

spec :: Spec
spec = do
  describe "TrackerList" $ do
    it "shuffleTiers (may fail with very small probability)" $ do
      list' <- shuffleTiers list
      list' `shouldSatisfy` (/= list)

    it "traverseAll" $ do
      xs <- traverseAll (\ (uri, _) -> if uri == L.last uris
               then throwIO (GenericException "")
               else return ()) list
      return ()

    it "traverseTiers" $ do
      xs' <- traverseTiers (\ (uri, _) -> if uri == L.last uris then return ()
                              else throwIO (GenericException "")) list

      return ()
