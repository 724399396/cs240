{-# LANGUAGE TemplateHaskell #-}

module Trahs where

import           Codec.Digest.SHA
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy     as L
import           Data.Int
import           Data.List
import qualified Data.Map.Strict          as Map
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.PosixCompat.Files
import           System.Process
import           System.Random

trassh :: String
trassh = "ssh -CTaxq @ ./trahs --server"

dbFile :: FilePath
dbFile = ".trahs.db"

type Directory = FilePath
type VersionInfos = Map.Map FilePath [VersionInfo]

data VersionInfo = VersionInfo {
    _replicaId :: Int64
  , _versionId :: Int64
  , _isDeleted :: Bool
  } deriving (Show, Read, Eq)

makeLenses ''VersionInfo

data FileInfo = FileInfo String deriving (Show, Read, Eq)

type FileInfos = Map.Map FilePath FileInfo

data DataBase = DataBase {
    _clientReplicaId :: Int64
  , _localVersion    :: Int64
  , _versionInfos    :: VersionInfos
  , _fileInfos       :: Map.Map FilePath FileInfo
  } deriving (Show, Read)

makeLenses ''DataBase

dataBase :: Directory -> IO DataBase
dataBase dir = do files <- getDirectoryContents dir
                  increaseVersion $ maybe initDB (fmap read . readFile) $ find (== dbFile) files
  where
    increaseVersion = fmap (over localVersion (+1))
    generateReplicaId = getStdRandom $ randomR (minBound:: Int64, maxBound :: Int64)
    initDB = generateReplicaId >>= (\rid -> return $ DataBase rid 0 Map.empty Map.empty)

fileInfo :: Directory -> IO (Map.Map FilePath FileInfo)
fileInfo dir = do
  files <- getDirectoryContents dir
  watchFile <- filterM (\f -> isFile f >>= return . (&& f /= dbFile)) files
  foldM insertFileInfo Map.empty watchFile
  where
    isFile f = isRegularFile <$> getSymbolicLinkStatus f
    fileHash path = showBSasHex <$> (hash SHA256 <$> L.readFile path)
    insertFileInfo infos nf = do info <- FileInfo <$> fileHash (dir </> nf)
                                 return $ Map.insert nf info infos

mergeInfo :: DataBase -> FileInfos -> DataBase
mergeInfo (DataBase rid lv vis fis) nfis =
  DataBase rid lv finalVis finalFis
  where
    added = Map.difference nfis fis
    updated = Map.filterWithKey (\k v -> fis Map.! k /= nfis Map.! k) $ Map.intersection fis nfis
    deleted = Map.difference fis nfis
    finalFis = Map.unions [added, updated]
    finalVis = Map.union (Map.foldrWithKey' (\k fi nvis -> Map.insert k (VersionInfo rid lv False) nvis) Map.empty finalFis) (Map.map (\v -> VersionInfo rid lv True) deleted)

server :: Handle -> Handle -> FilePath -> IO ()
server r w dir = do
  hPutStrLn w "I am the server"
  line <- hGetLine r
  -- maybe turn to client mode
  hPutStrLn w $ "You said " ++ line

client :: Bool -> Handle -> Handle -> FilePath -> IO ()
client turn r w dir = do
  line <- hGetLine r
  hPutStrLn stderr $ "The server said " ++ show line
  hPutStrLn w "Hello, server"
  line' <- hGetLine r
  hPutStrLn stderr $ "The server said " ++ show line'
  -- if turn, turn to server

hostCmd :: String -> FilePath -> IO String
hostCmd host dir = do
  tmp <- maybe trassh id <$> lookupEnv "TRASSH"
  case break (== '@') tmp of
    (b, '@':e) -> return $ b ++ host ++ e ++ ' ':dir
    _          -> return $ tmp ++ ' ':dir

spawnRemote :: String -> FilePath -> IO (Handle, Handle)
spawnRemote host dir = do
  cmd <- hostCmd host dir
  hPutStrLn stderr $ "running " ++ show cmd
  (Just w, Just r, _, _) <- createProcess (shell cmd) {
      std_in = CreatePipe
    , std_out = CreatePipe
    }
  hSetBuffering w LineBuffering
  return (r, w)

connect :: String -> FilePath -> FilePath -> IO ()
connect host rdir ldir = do
  (r, w) <- spawnRemote host rdir
  client True r w ldir

trahs :: IO ()
trahs = do
  args <- getArgs
  case args of
    ["--server", l] -> do hSetBuffering stdout LineBuffering
                          server stdin stdout l
    [r, l] | (host, ':':rdir) <- break (== ':') r -> connect host rdir l
    _ -> do hPutStrLn stderr "usage: trahs HOST:DIR LOCALDIR"
            exitFailure

