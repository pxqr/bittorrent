{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -fno-warn-orphans   #-}
module Main (main) where

import Prelude as P
import Control.Exception
import Control.Lens hiding (argument)
import Control.Monad
import Data.List as L
import Data.Monoid
import Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read as T
import Data.Version
import Network.URI
import Options.Applicative
import System.Log
import System.Log.Logger
import System.Exit
import Text.Read
import Text.PrettyPrint.Class

import Paths_bittorrent (version)
import Data.Torrent
import Data.Torrent.Magnet hiding (Magnet (Magnet))
import Data.Torrent.Magnet (Magnet)


{-----------------------------------------------------------------------
--  Dialogs
-----------------------------------------------------------------------}

instance Read URI where
  readsPrec _ = f . parseURI
    where
      f Nothing  = []
      f (Just u) = [(u, "")]

question :: Show a => Text -> Maybe a -> IO ()
question q def = do
  T.putStrLn q
  case def of
    Nothing -> return ()
    Just v  -> T.putStrLn $ "[default: " <> T.pack (show v) <> "]"

ask :: Read a => Text -> IO a
ask q = question q (Just True) >> getReply
  where
    getReply = do
      resp <- P.getLine
      maybe getReply return $ readMaybe resp

askMaybe :: Read a => Text -> IO (Maybe a)
askMaybe q = question q (Just False) >> getReply
  where
    getReply = do
      resp <- P.getLine
      if resp == []
        then return Nothing
        else maybe getReply return $ readMaybe resp

askURI :: IO URI
askURI = do
  str <- P.getLine
  case parseURI str of
    Nothing -> T.putStrLn "incorrect URI" >> askURI
    Just u  -> return u

askFreeform :: IO Text
askFreeform = do
  str <- T.getLine
  if T.null str
    then askFreeform
    else return str

askInRange :: Int -> Int -> IO Int
askInRange a b = do
  str <- T.getLine
  case T.decimal str of
    Left msg -> do
      P.putStrLn msg
      askInRange a b
    Right (i, _)
      | a <= i && i < b -> return i
      |     otherwise   -> do
        T.putStrLn "not in range "
        askInRange a b

askChoice :: [(Text, a)] -> IO a
askChoice kvs = do
  forM_ (L.zip [1 :: Int ..] $ L.map fst kvs) $ \(i, lbl) -> do
    T.putStrLn $ "  " <> T.pack (show i) <> ") " <> lbl
  T.putStrLn "Your choice?"
  ix <- askInRange 1 (succ (L.length kvs))
  return $ snd (kvs !! pred ix)

{-----------------------------------------------------------------------
--  Helpers
-----------------------------------------------------------------------}

torrentFile :: Parser FilePath
torrentFile = argument Just
    ( metavar "FILE"
   <> help    "A .torrent file"
    )

{-----------------------------------------------------------------------
--  Amend command - edit a field of torrent file
-----------------------------------------------------------------------}

data AmendOpts = AmendOpts FilePath
  deriving Show

amendInfo :: ParserInfo AmendOpts
amendInfo = info (helper <*> parser) modifier
  where
    modifier = progDesc "Edit info fields of existing torrent"
    parser   = AmendOpts <$> torrentFile

type Amend = Torrent -> Torrent

fields :: [(Text, IO Amend)]
fields = [ ("announce",      set announce            <$> askURI)
         , ("comment",       set comment      . Just <$> askFreeform)
         , ("created by",    set createdBy    . Just <$> askFreeform)
         , ("publisher url", set publisherURL . Just <$> askURI)
         ]

askAmend :: IO Amend
askAmend = join $ T.putStrLn "Choose a field:" >> askChoice fields

amend :: AmendOpts -> IO ()
amend (AmendOpts tpath) = do
  t <- fromFile tpath
  a <- askAmend
  toFile tpath $ a t

{-----------------------------------------------------------------------
--  Check command
-----------------------------------------------------------------------}

{-
checkOpts :: Parser CheckOpts
checkOpts = CheckOpts
    <$> torrentFile
    <*> argument Just
        ( metavar "PATH"
       <> value   "."
       <> help    "Content directory or a single file" )

checkInfo :: ParserInfo CheckOpts
checkInfo = info (helper <*> checkOpts) modifier
  where
    modifier = progDesc "Validate integrity of torrent data"

{-----------------------------------------------------------------------
--  Create command
-----------------------------------------------------------------------}

createFlags :: Parser CreateFlags
createFlags = CreateFlags
    <$> optional (option
      ( long    "piece-size"
     <> short   's'
     <> metavar "SIZE"
     <> help    "Set size of torrent pieces"
      ))
    <*> switch
      ( long    "md5"
     <> short   '5'
     <> help    "Include md5 hash of each file"
      )
    <*> switch
      ( long    "ignore-dot-files"
     <> short   'd'
     <> help    "Do not include .* files"
      )


createOpts :: Parser CreateOpts
createOpts = CreateOpts
    <$> argument Just
        ( metavar "PATH"
       <> help    "Content directory or a single file"
        )
    <*> optional (argument Just
        ( metavar "FILE"
       <> help    "Place for the output .torrent file"
        ))
    <*> createFlags

createInfo :: ParserInfo CreateOpts
createInfo = info (helper <*> createOpts) modifier
  where
    modifier = progDesc "Make a new .torrent file"
-}

{-----------------------------------------------------------------------
--  Magnet command -- print magnet link for given torrent file
-----------------------------------------------------------------------}

data MagnetOpts = MagnetOpts
  { magnetFile :: FilePath -- ^ path to torrent file
  , detailed   :: Bool     -- ^ whether to append additional uri params
  } deriving Show

magnetInfo :: ParserInfo MagnetOpts
magnetInfo = info (helper <*> parser) modifier
  where
    modifier = progDesc "Print magnet link"
    parser   = MagnetOpts
      <$> torrentFile
      <*> switch ( long "detailed" )

magnet :: MagnetOpts -> IO ()
magnet MagnetOpts {..} = print . magnetLink =<< fromFile magnetFile
  where
    magnetLink = if detailed then detailedMagnet else simpleMagnet

{-----------------------------------------------------------------------
--  Show command - print torrent file information
-----------------------------------------------------------------------}

data ShowOpts = ShowOpts
  { showPath     :: FilePath -- ^ torrent file to inspect;
  , infoHashOnly :: Bool     -- ^ omit everything except infohash.
  } deriving Show

showInfo :: ParserInfo ShowOpts
showInfo = info (helper <*> parser) modifier
  where
    modifier = progDesc "Print .torrent file metadata"
    parser   = ShowOpts
      <$> torrentFile
      <*> switch
          ( long "infohash"
         <> help "Show only hash of the torrent info part"
          )

showTorrent :: ShowOpts -> Torrent -> ShowS
showTorrent ShowOpts {..} torrent
  | infoHashOnly = shows $ idInfoHash (tInfoDict torrent)
  |   otherwise  = shows $ pretty torrent

putTorrent :: ShowOpts -> IO ()
putTorrent opts @ ShowOpts {..} = do
    torrent <- fromFile showPath `onException` putStrLn help
    putStrLn $ showTorrent opts torrent []
  where
    help = "Most likely this is not a valid .torrent file"

{-----------------------------------------------------------------------
--  Command
-----------------------------------------------------------------------}

data Command
  = Amend  AmendOpts
--  | Check  CheckOpts
--  | Create CreateOpts
  | Magnet MagnetOpts
  | Show   ShowOpts
    deriving Show

commandOpts :: Parser Command
commandOpts = subparser $ mconcat
    [ command "amend"  (Amend  <$> amendInfo)
--    , command "check"  (Check  <$> checkInfo)
--    , command "create" (Create <$> createInfo)
    , command "magnet" (Magnet <$> magnetInfo)
    , command "show"   (Show   <$> showInfo)
    ]

{-----------------------------------------------------------------------
--  Global Options
-----------------------------------------------------------------------}

data GlobalOpts = GlobalOpts
  { verbosity   :: Priority
  } deriving Show

deriving instance Enum    Priority
deriving instance Bounded Priority

priorities :: [Priority]
priorities = [minBound..maxBound]

defaultPriority :: Priority
defaultPriority = WARNING

verbosityOpts :: Parser Priority
verbosityOpts = verbosityP <|> verboseP <|> quietP
  where
    verbosityP = option
      ( long    "verbosity"
     <> metavar "LEVEL"
     <> help   ("Set verbosity level\n"
             ++ "Possible values are " ++ show priorities)
      )

    verboseP = flag defaultPriority INFO
      ( long    "verbose"
     <> short   'v'
     <> help    "Verbose mode"
      )

    quietP = flag defaultPriority CRITICAL
      ( long    "quiet"
     <> short   'q'
     <> help    "Silent mode"
      )


globalOpts :: Parser GlobalOpts
globalOpts = GlobalOpts <$> verbosityOpts

data Options = Options
  { cmdOpts  :: Command
  , globOpts :: GlobalOpts
  } deriving Show

options :: Parser Options
options = Options <$> commandOpts <*> globalOpts

versioner :: String -> Version -> Parser (a -> a)
versioner prog ver = nullOption $ mconcat
    [ long  "version"
    , help  "Show program version and exit"
    , value id
    , metavar ""
    , hidden
    , reader $ const $ undefined -- Left $ ErrorMsg versionStr
    ]
  where
    versionStr = prog ++ " version " ++ showVersion ver

parserInfo :: ParserInfo Options
parserInfo = info parser modifier
  where
    parser   = helper <*> versioner "mktorrent" version <*> options
    modifier = header synopsis <> progDesc description <> fullDesc
    synopsis    = "Torrent management utility"
    description = "" -- TODO

{-----------------------------------------------------------------------
--  Dispatch
-----------------------------------------------------------------------}

run :: Command -> IO ()
run (Amend  opts) = amend opts
--run (Check  opts) = checkTorrent opts
--run (Create opts) = createTorrent opts
run (Magnet opts) = magnet opts
run (Show   opts) = putTorrent opts

prepare :: GlobalOpts -> IO ()
prepare GlobalOpts {..} = do
  updateGlobalLogger rootLoggerName (setLevel verbosity)

main :: IO ()
main = do
  Options {..} <- execParser parserInfo
  prepare globOpts
  run cmdOpts
