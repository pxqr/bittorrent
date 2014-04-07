{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS -fno-warn-orphans           #-}
module Data.TorrentSpec (spec) where
import Control.Applicative
import Data.BEncode
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Convertible
import Data.Maybe
import Data.Monoid
import Data.Time
import Network.URI
import System.FilePath
import System.Posix.Types
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Torrent
import Network.BitTorrent.CoreSpec ()


pico :: Gen (Maybe NominalDiffTime)
pico = oneof
  [ pure Nothing
  , (Just . fromIntegral) <$> (arbitrary :: Gen Int)
  ]

instance Arbitrary COff where
  arbitrary = fromIntegral <$> (arbitrary :: Gen Int)

instance Arbitrary URIAuth where
  arbitrary = URIAuth <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary URI where
  arbitrary
    = pure $ fromJust $ parseURI "http://ietf.org/1737.txt?a=1&b=h#123"

instance Arbitrary InfoHash where
  arbitrary = do
    bs <- BS.pack <$> vectorOf 20 arbitrary
    pure $ either (const (error "arbitrary infohash")) id $ safeConvert bs

instance Arbitrary a => Arbitrary (FileInfo a) where
  arbitrary = FileInfo <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LayoutInfo where
  arbitrary = oneof
    [ SingleFile <$> arbitrary
    , MultiFile  <$> arbitrary <*> arbitrary
    ]

instance Arbitrary a => Arbitrary (Piece a) where
  arbitrary = Piece <$> arbitrary <*> arbitrary

instance Arbitrary HashList where
  arbitrary = HashList <$> arbitrary

instance Arbitrary PieceInfo where
  arbitrary = PieceInfo <$> arbitrary <*> arbitrary

instance Arbitrary InfoDict where
  arbitrary = infoDictionary <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Torrent where
  arbitrary = Torrent <$> arbitrary
                 <*> arbitrary <*> arbitrary    <*> arbitrary
                 <*> pico      <*> arbitrary    <*> arbitrary
                 <*> arbitrary
                 <*> arbitrary <*> pure Nothing <*> arbitrary

instance Arbitrary Magnet where
  arbitrary = Magnet <$> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> pure mempty

type TestPair = (FilePath, String)

-- TODO add a few more torrents here
torrentList :: [TestPair]
torrentList =
  [ ( "res" </> "dapper-dvd-amd64.iso.torrent"
    , "0221caf96aa3cb94f0f58d458e78b0fc344ad8bf")
  ]

infohashSpec :: (FilePath, String) -> Spec
infohashSpec (filepath, expectedHash) = do
  it ("should match " ++ filepath) $ do
    torrent    <- fromFile filepath
    let actualHash = show $ idInfoHash $ tInfoDict torrent
    actualHash `shouldBe` expectedHash

magnetEncoding :: Magnet -> IO ()
magnetEncoding m = parseMagnet (renderMagnet m) `shouldBe` Just m

data T a = T

prop_properBEncode :: Show a => BEncode a => Eq a
                   => T a -> a -> IO ()
prop_properBEncode _ expected = actual `shouldBe` Right expected
  where
    actual = decode $ BL.toStrict $ encode expected

spec :: Spec
spec = do
  describe "info hash" $ do
    mapM_ infohashSpec torrentList

  describe "accumPosition" $ do
    it "" $ property $ \ p1 p2 p3 s1 s2 s3 ->
      accumPositions [(p1, s1), (p2, s2), (p3, s3)]
        `shouldBe`   [(p1, (0, s1)), (p2, (s1, s2)), (p3, (s1 + s2, s3))]

  describe "FileInfo" $ do
    it "properly bencoded" $ property $
      prop_properBEncode (T :: T (FileInfo BS.ByteString))

  describe "LayoutInfo" $ do
    it "properly bencoded" $ property $
      prop_properBEncode (T :: T LayoutInfo)

  describe "Torrent" $ do
    it "property bencoded" $ property $
      prop_properBEncode (T :: T Torrent)

  describe "Magnet" $ do
    it "properly encoded" $ property $ magnetEncoding

    it "parse base32" $ do
      let magnet = "magnet:?xt=urn:btih:CT76LXJDDCH5LS2TUHKH6EUJ3NYKX4Y6"
      let ih = "CT76LXJDDCH5LS2TUHKH6EUJ3NYKX4Y6"
      parseMagnet magnet `shouldBe` Just (nullMagnet ih)

    it "parse base16" $ do
      let magnet = "magnet:?xt=urn:btih:0123456789abcdef0123456789abcdef01234567"
      let ih = "0123456789abcdef0123456789abcdef01234567"
      parseMagnet magnet `shouldBe` Just (nullMagnet ih)
