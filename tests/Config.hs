{-# LANGUAGE RecordWildCards #-}
module Config
       ( -- * Types
         ClientName
       , ClientOpts (..)
       , EnvOpts (..)

         -- * For test suite driver
       , getOpts

         -- * For item specs
       , getEnvOpts
       , getThisOpts
       , getRemoteOpts
       , withRemote
       ) where

import Control.Monad
import Network
import Data.Default
import Data.IORef
import Data.List as L
import Data.Maybe
import Options.Applicative
import System.Exit
import System.Environment
import System.IO.Unsafe
import Test.Hspec

type ClientName = String


instance Read PortNumber where
  readsPrec = undefined

data ClientOpts = ClientOpts
  { peerPort   :: PortNumber  -- tcp port
  , nodePort   :: PortNumber  -- udp port
  }

instance Default ClientOpts where
  def = ClientOpts
    { peerPort = 6881
    , nodePort = 6881
    }

defRemoteOpts :: ClientOpts
defRemoteOpts = def

defThisOpts :: ClientOpts
defThisOpts = def
  { peerPort = 6882
  , nodePort = 6882
  }

clientOptsParser :: Parser ClientOpts
clientOptsParser = ClientOpts
  <$> option
    ( long    "peer-port"  <> short 'p'
   <> value   6881         <> showDefault
   <> metavar "NUM"
   <> help    "port to bind the specified bittorrent client"
    )
  <*> option
    ( long    "node-port"  <> short 'n'
   <> value   6881         <> showDefault
   <> metavar "NUM"
   <> help    "port to bind node of the specified client"
    )

data EnvOpts = EnvOpts
  { testClient :: Maybe ClientName
  , remoteOpts :: ClientOpts
  , thisOpts   :: ClientOpts
  }

instance Default EnvOpts where
  def = EnvOpts
    { testClient = Nothing
    , remoteOpts = defRemoteOpts
    , thisOpts   = defThisOpts
    }

findConflicts :: EnvOpts -> [String]
findConflicts EnvOpts {..}
  | isNothing testClient = []
  | peerPort remoteOpts == peerPort thisOpts = ["Peer port the same"]
  | nodePort remoteOpts == nodePort thisOpts = ["Node port the same"]
  |         otherwise        = []


envOptsParser :: Parser EnvOpts
envOptsParser = EnvOpts
  <$> optional (strOption
    ( long    "bittorrent-client"
   <> metavar "CLIENT"
   <> help    "torrent client to run"
    ))
  <*> clientOptsParser
  <*> clientOptsParser

envOptsInfo :: ParserInfo EnvOpts
envOptsInfo = info (helper <*> envOptsParser)
   ( fullDesc
  <> progDesc "The bittorrent library testsuite"
  <> header   ""
   )

-- do not modify this while test suite is running because spec items
-- can run in parallel
envOptsRef :: IORef EnvOpts
envOptsRef = unsafePerformIO (newIORef def)

-- | Should be used from spec items.
getEnvOpts :: IO EnvOpts
getEnvOpts = readIORef envOptsRef

getThisOpts :: IO ClientOpts
getThisOpts = thisOpts <$> getEnvOpts

-- | Return 'Nothing' if remote client is not running.
getRemoteOpts :: IO (Maybe ClientOpts)
getRemoteOpts = return Nothing

withRemote :: (ClientOpts -> Expectation) -> Expectation
withRemote action = do
  mopts <- getRemoteOpts
  case mopts of
    Nothing   -> pendingWith "Remote client isn't running"
    Just opts -> action opts

-- TODO fix EnvOpts parsing

-- | Should be used by test suite driver.
getOpts :: IO (EnvOpts, [String])
getOpts = do
  args <- getArgs
--  case runParser SkipOpts envOptsParser args) (prefs idm) of
  case (Right (def, args), ()) of
    (Left   err                , _ctx) -> exitFailure
    (Right (envOpts, hspecOpts), _ctx) -> do
      let conflicts = findConflicts envOpts
      unless (L.null conflicts) $ do
        forM_ conflicts putStrLn
        exitFailure

      writeIORef envOptsRef envOpts
      return (envOpts, hspecOpts)