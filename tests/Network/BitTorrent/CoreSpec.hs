-- | Re-export modules.
module Network.BitTorrent.CoreSpec (spec) where
import Network.BitTorrent.Core.PeerIdSpec as CoreSpec ()

import Test.Hspec (Spec)

spec :: Spec
spec = return ()