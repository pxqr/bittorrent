{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Conduit as C
import Data.Conduit.List as C
import Data.Default
import Data.Maybe
import Network.URI
import Options.Applicative
import System.Exit
import System.FilePath

import Data.Torrent
import Data.Torrent.InfoHash
import Network.BitTorrent.Core
import Network.BitTorrent.DHT.Session
import Network.BitTorrent.DHT as DHT
import Network.BitTorrent.Exchange.Message
import Network.BitTorrent.Exchange.Wire


data Params = Params
  { topic    :: InfoHash
  , thisNode :: NodeAddr IPv4
  , bootNode :: NodeAddr IPv4
  , buckets  :: Int
  } deriving Show

paramsParser :: Parser Params
paramsParser = Params
  <$> option (long    "infohash" <> short 'i'
           <> metavar "SHA1"     <> help "infohash of torrent file")
  <*> option (long    "port"     <> short 'p'
           <> value def          <> showDefault
           <> metavar "NUM"      <> help "port number to bind"
             )
  <*> option (long    "boot"     <> short 'b'
           <> metavar "NODE"     <> help "bootstrap node address"
             )
  <*> option (long    "bucket"   <> short 'n'
           <> value 2            <> showDefault
           <> metavar "NUM"      <> help "number of buckets to maintain"
             )

programInfo :: ParserInfo Params
programInfo = info (helper <*> paramsParser)
   ( fullDesc
  <> progDesc ""
  <> header   "gettorrent - get torrent file by infohash"
   )

exchangeTorrent :: PeerAddr IP -> InfoHash -> IO InfoDict
exchangeTorrent addr ih = do
  pid <- genPeerId
  var <- newEmptyMVar
  let hs = Handshake def (toCaps [ExtExtended]) ih pid
  connectWire hs addr (toCaps [ExtMetadata]) $ do
    infodict <- getMetadata
    liftIO $ putMVar var infodict
  takeMVar var

getTorrent :: Params -> IO ()
getTorrent Params {..} = do
  dht (def { optBucketCount = buckets }) thisNode $ do
    bootstrap [bootNode]
    DHT.lookup topic $$ C.mapM_ $ \ peers -> do
      liftIO $ forM_ peers $ \ peer -> do
        infodict <- exchangeTorrent (IPv4 <$> peer) topic
        let torrent = nullTorrent infodict -- TODO add tNodes, tCreated, etc?
        toFile (show topic <.> torrentExt) torrent
        exitSuccess

main :: IO ()
main = execParser programInfo >>= getTorrent
