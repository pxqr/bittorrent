module Network.BitTorrent.Core.PeerIdSpec (spec) where
import Control.Applicative
import Test.Hspec
import Test.QuickCheck
import Network.BitTorrent.Core.PeerId


instance Arbitrary PeerId where
  arbitrary = oneof
    [ azureusStyle defaultClientId defaultVersionNumber <$> arbitrary
    , shadowStyle  'X'             defaultVersionNumber <$> arbitrary
    ]

spec :: Spec
spec = return ()