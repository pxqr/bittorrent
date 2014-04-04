{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Torrent.PieceSpec (spec) where
import Control.Applicative
import Test.Hspec
import Test.QuickCheck
import Data.Torrent


instance Arbitrary a => Arbitrary (Piece a) where
  arbitrary = Piece <$> arbitrary <*> arbitrary

spec :: Spec
spec = return ()