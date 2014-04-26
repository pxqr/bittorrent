{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.BitTorrent.DHT.TokenSpec (spec) where
import Control.Applicative
import Data.List as L
import Data.String
import Test.Hspec
import Test.QuickCheck

import Network.BitTorrent.Address
import Network.BitTorrent.CoreSpec ()
import Network.BitTorrent.DHT.Token as T


instance Arbitrary Token where
  arbitrary = fromString <$> arbitrary

instance Arbitrary TokenMap where
  arbitrary = tokens <$> arbitrary

repeatN :: Int -> (a -> a) -> (a -> a)
repeatN n f = L.foldr (.) id $ L.replicate n f

spec :: Spec
spec = do
  describe "Token" $ do
    return ()

  describe "TokenMap" $ do
    it "is keeping any granted token in current session" $
      property $ \ (addr :: NodeAddr IPv4) m ->
        T.member addr (T.lookup addr m) m

    it "is keeping any granted token in next session" $
      property $ \ (addr :: NodeAddr IPv4) m ->
        T.member addr (T.lookup addr m) (T.update m)

    -- can fail with some small probability
    it "is rejecting any outdated tokens" $
      property $ \ (addr :: NodeAddr IPv4) m k -> not $
        let n = min 100 (abs k + 2) in
        T.member addr (T.lookup addr m) (repeatN n T.update m)