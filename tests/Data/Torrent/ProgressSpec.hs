{-# OPTIONS -fno-warn-orphans #-}
module Data.Torrent.ProgressSpec (spec) where
import Control.Applicative
import Test.Hspec
import Test.QuickCheck
import Data.Torrent.Progress


instance Arbitrary Progress where
  arbitrary = Progress <$> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = return ()
