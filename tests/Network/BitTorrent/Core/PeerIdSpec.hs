module Network.BitTorrent.Core.PeerIdSpec (spec) where
import Control.Applicative
import Data.Text.Encoding as T
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Network.BitTorrent.Core.PeerId


instance Arbitrary PeerId where
  arbitrary = oneof
    [ azureusStyle defaultClientId defaultVersionNumber
        <$> pure ""
--    , shadowStyle  'X'             defaultVersionNumber
--        <$> (T.encodeUtf8 <$> arbitrary)
    ]

spec :: Spec
spec = return ()