module Main (main) where

import Data.Torrent
import Network.Torrent.THP
import System.Environment
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Data.BEncode

main :: IO ()
main = do
  [path] <- getArgs
  contents <- B.readFile path

  let Right contents' = decode contents >>= return . L.toStrict . encode
  print (contents' == contents)
--  let (a, b) = showInfos contents
--  print b
--  print a
--  print (encode b == encoded a)

  let Right b = decode contents
  let Right t = fromBEncode b

  let req = defaultRequest (tAnnounce t) (tInfoHash t)
  resp <- sendRequest req
  print resp
