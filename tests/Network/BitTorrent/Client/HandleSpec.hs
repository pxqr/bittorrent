module Network.BitTorrent.Client.HandleSpec (spec) where
import Data.Default
import Test.Hspec

import Data.Torrent
import Network.BitTorrent.Client
import Network.BitTorrent.Client.Handle

data_dir :: FilePath
data_dir = "data"

spec :: Spec
spec = do
  describe "openMagnet" $ do
    it "should add new infohash to index" $ do
      simpleClient $ do
        _ <- openMagnet data_dir (nullMagnet def)
        _ <- getHandle def
        return ()
