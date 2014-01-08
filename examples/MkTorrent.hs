{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -fno-warn-orphans   #-}
module Main (main) where

import Prelude as P
import Control.Concurrent.ParallelIO
import Control.Exception
import Control.Lens hiding (argument)
import Control.Monad
import Data.List as L
import Data.Maybe
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
import Data.Torrent.Bitfield as BF
import Data.Torrent.Piece
import Data.Torrent.Layout
import Data.Torrent.Magnet hiding (Magnet)
import System.Torrent.Storage


{-----------------------------------------------------------------------
--  Dialogs
-----------------------------------------------------------------------}

instance Read URI where
  readsPrec _ = f . parseURI
    where
      f Nothing  = []
      f (Just u) = [(u, "")]

question :: Show a => Text -> Maybe a -> IO ()
question q defVal = do
  T.putStrLn q
  case defVal of
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
  s <- P.getLine
  case parseURI s of
    Nothing -> T.putStrLn "incorrect URI" >> askURI
    Just u  -> return u

askFreeform :: IO Text
askFreeform = do
  s <- T.getLine
  if T.null s
    then askFreeform
    else return s

askInRange :: Int -> Int -> IO Int
askInRange a b = do
  s <- T.getLine
  case T.decimal s of
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
  n <- askInRange 1 (succ (L.length kvs))
  return $ snd (kvs !! pred n)

{-----------------------------------------------------------------------
--  Helpers
-----------------------------------------------------------------------}

torrentFile :: Parser FilePath
torrentFile = argument Just
    ( metavar "TORRENT_FILE_PATH"
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
fields = [ ("announce",      set announce     . Just <$> askURI)
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
--  Check command -- validate content files using torrent file
-----------------------------------------------------------------------}

data CheckOpts = CheckOpts
  { checkTorrentPath :: FilePath -- ^ validation torrent file
  , checkContentPath :: FilePath -- ^ root dir for content files
  } deriving Show

checkInfo :: ParserInfo CheckOpts
checkInfo = info (helper <*> parser) modifier
  where
    modifier = progDesc "Validate integrity of torrent data"
        <> header   "append +RTS -N$NUMBER_OF_CORES -RTS for parallel execution"
    parser   = CheckOpts
      <$> torrentFile
      <*> argument Just
          ( metavar "CONTENT_DIR_PATH"
         <> value   "."
         <> help    "Content directory or a single file"
          )

validatePiece :: Storage -> PieceInfo -> PieceIx -> IO (Maybe PieceIx)
validatePiece s pinfo pix = do
  valid <- verifyPiece s pinfo pix
  if valid
    then do infoM "check" $ "valid piece "   ++ show pix
            return (Just pix)
    else do infoM "check" $ "invalid piece " ++ show pix
            return Nothing

validateStorage :: Storage -> PieceInfo -> IO Bitfield
validateStorage s pinfo = do
  infoM "check" "start storage validation"
  let total = totalPieces s
  pixs <- parallel $ L.map (validatePiece s pinfo) [0 .. total - 1]
  infoM "check" "storage validation finished"
  return $ fromList total $ catMaybes pixs

-- TODO use local thread pool
checkContent :: Storage -> PieceInfo -> IO ()
checkContent s pinfo = do
  invalids <- BF.complement <$> validateStorage s pinfo
  if BF.null invalids
    then noticeM "check" "all files are complete and valid"
    else do
      emergencyM "check" $ "there are some invalid pieces" ++ show invalids
      exitFailure

checkTorrent :: CheckOpts -> IO ()
checkTorrent CheckOpts {..} = do
  InfoDict {..} <- tInfoDict <$> fromFile checkTorrentPath
  let layout = flatLayout checkContentPath idLayoutInfo
  withStorage ReadOnly (piPieceLength idPieceInfo) layout $ \ s -> do
    infoM "check" "files mapped"
    checkContent s idPieceInfo
    infoM "check" "unmapping files"

{-----------------------------------------------------------------------
--  Create command
-----------------------------------------------------------------------}
{-
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
    torrent <- fromFile showPath `onException` putStrLn msg
    putStrLn $ showTorrent opts torrent []
  where
    msg = "Torrent file is either invalid or do not exist"

{-----------------------------------------------------------------------
--  Command
-----------------------------------------------------------------------}

data Command
  = Amend  AmendOpts
  | Check  CheckOpts
--  | Create CreateOpts
  | Magnet MagnetOpts
  | Show   ShowOpts
    deriving Show

commandOpts :: Parser Command
commandOpts = subparser $ mconcat
    [ command "amend"  (Amend  <$> amendInfo)
    , command "check"  (Check  <$> checkInfo)
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
run (Check  opts) = checkTorrent opts
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