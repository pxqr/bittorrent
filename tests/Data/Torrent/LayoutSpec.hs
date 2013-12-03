{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Data.Torrent.LayoutSpec (spec) where

import Control.Applicative
import Test.Hspec
import Test.QuickCheck
import System.Posix.Types

import Data.Torrent.Layout


instance Arbitrary COff where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Int)

instance Arbitrary a => Arbitrary (FileInfo a) where
  arbitrary = FileInfo <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LayoutInfo where
  arbitrary = oneof
    [ SingleFile <$> arbitrary
    , MultiFile  <$> arbitrary <*> arbitrary
    ]

spec :: Spec
spec = do
  describe "accumPosition" $ do
    it "" $ property $ \ p1 p2 p3 s1 s2 s3 ->
      accumPositions [(p1, s1), (p2, s2), (p3, s3)]
        `shouldBe`   [(p1, (0, s1)), (p2, (s1, s2)), (p3, (s1 + s2, s3))]