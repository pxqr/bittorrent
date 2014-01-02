module Main (main) where
import Data.Default

import Options.Applicative
import Data.Torrent.InfoHash
import Network.BitTorrent.Core


data Params = Params
  { infohash :: InfoHash
  , thisNode :: NodeAddr IPv4
  , bootNode :: NodeAddr IPv4
  } deriving Show

paramsParser :: Parser Params
paramsParser = Params
  <$> option (long    "infohash" <> short 'i'
           <> metavar "SHA1"     <> help "infohash of torrent file")
  <*> option (long    "node"     <> short 'n' <> value def
           <> metavar "NODE"     <> help "this node address"
             )
  <*> option (long    "boot"     <> short 'b'
           <> metavar "NODE"     <> help "bootstrap node address"
             )

programInfo :: ParserInfo Params
programInfo = info (helper <*> paramsParser)
   ( fullDesc
  <> progDesc ""
  <> header   "gettorrent - get torrent file by infohash"
   )

getTorrent :: Params -> IO ()
getTorrent = print

main :: IO ()
main = execParser programInfo >>= getTorrent
