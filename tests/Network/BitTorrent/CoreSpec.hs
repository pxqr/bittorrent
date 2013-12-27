-- | Re-export modules.
module Network.BitTorrent.CoreSpec (spec) where
import Network.BitTorrent.Core.FingerprintSpec as CoreSpec ()
import Network.BitTorrent.Core.NodeSpec as CoreSpec ()
import Network.BitTorrent.Core.PeerIdSpec as CoreSpec ()
import Network.BitTorrent.Core.PeerAddrSpec as CoreSpec ()

import Test.Hspec (Spec)

spec :: Spec
spec = return ()