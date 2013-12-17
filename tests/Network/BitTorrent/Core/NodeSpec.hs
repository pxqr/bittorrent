module Network.BitTorrent.Core.NodeSpec (spec) where
import Control.Applicative
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Core.Node
import Network.BitTorrent.Core.PeerAddrSpec ()

instance Arbitrary NodeId where
  arbitrary = undefined

instance Arbitrary a => Arbitrary (NodeAddr a) where
  arbitrary = NodeAddr <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (NodeInfo a) where
  arbitrary = NodeInfo <$> arbitrary <*> arbitrary

spec :: Spec
spec = return ()