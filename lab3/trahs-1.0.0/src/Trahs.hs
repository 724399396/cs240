{-# LANGUAGE TemplateHaskell #-}

module Trahs where

import           Codec.Digest.SHA
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy     as L
import           Data.Int
import           Data.List
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromJust)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.IO.Temp
import           System.PosixCompat.Files
import           System.Process
import           System.Random

trassh :: String
trassh = "ssh -CTaxq @ ./trahs --server"

dbFile :: FilePath
dbFile = ".trahs.db"

type Directory = FilePath

data VersionInfo = VersionInfo {
    _replicaId :: Int64
  , _versionId :: Int64
  , _isDeleted :: Bool
  } deriving (Show, Read, Eq)

makeLenses ''VersionInfo

data FileInfo = FileInfo String deriving (Show, Read, Eq)

type VersionInfos = Map.Map FilePath [VersionInfo]
type FileInfos = Map.Map FilePath FileInfo

data Database = Database {
    _clientReplicaId :: Int64
  , _localVersion    :: Int64
  , _versionInfos    :: VersionInfos
  , _fileInfos       :: FileInfos
  } deriving (Show, Read, Eq)

makeLenses ''Database

-- sync local db
dataBase :: Directory -> IO Database
dataBase dir = do files <- getDirectoryContents dir
                  increaseVersion $ maybe initDB (fmap read . readFile . (dir </>)) $ find (== dbFile) files
  where
    increaseVersion = fmap (over localVersion (+1))
    generateReplicaId = getStdRandom $ randomR (minBound:: Int64, maxBound :: Int64)
    initDB = generateReplicaId >>= (\rid -> return $ Database rid 0 Map.empty Map.empty)

fileInfo :: Directory -> IO FileInfos
fileInfo dir = do
  files <- getDirectoryContents dir
  watchFile <- filterM (\f -> isFile (dir </> f) >>= return . (&& f /= dbFile)) files
  Map.fromList <$> (mapM (\f -> fileHash (dir </> f) >>= \h -> return (f, FileInfo h)) watchFile)
  where
    isFile f = isRegularFile <$> getSymbolicLinkStatus f
    fileHash path = showBSasHex <$> (hash SHA256 <$> L.readFile path)

mergeInfo :: Database -> FileInfos -> Database
mergeInfo (Database rid lv vis fis) nfis =
  Database rid lv finalVis finalFis
  where
    added = Map.difference nfis fis
    oringalExisted = Map.intersection nfis fis
    updated = Map.filterWithKey (\k _ -> fis Map.! k /= nfis Map.! k) oringalExisted
    deleted = Map.difference fis nfis
    finalFis = Map.unions [added, oringalExisted]
    getVersion :: FilePath -> Maybe VersionInfo -> VersionInfo
    getVersion f vi = if (Map.member f updated)
                      then VersionInfo rid lv False
                      else if (Map.member f deleted)
                           then VersionInfo rid lv True
                           else (fromJust vi)
    addedVis = Map.map (const [VersionInfo rid lv False]) added
    updateVersion f ovi =
      let otherReplicaVis = filter (\vi -> vi^.replicaId /= rid) ovi
          modifiedVis = getVersion f (find (\vi -> vi^.replicaId == rid) ovi)
      in modifiedVis : otherReplicaVis
    existedThenUpdateVis = Map.mapWithKey updateVersion vis
    finalVis = Map.union addedVis existedThenUpdateVis

syncLocalDb :: FilePath -> IO Database
syncLocalDb dir = do db <- dataBase dir
                     fi <- fileInfo dir
                     return $ mergeInfo db fi

syncDbToDisk :: FilePath -> Database -> IO ()
syncDbToDisk dir db =
  do withTempFile dir dbFile $ (\nPath h ->
         do hPutStr h (show db)
            renameFile nPath (dir </> dbFile))

sendDbToClient :: Database -> Handle -> IO ()
sendDbToClient db h = hPutStrLn h (show db)

data Diff = Diff {
  _addedFiles: [FilePath],
  _updatedFiles: [FilePath],
  _deleteFiles: [FilePath],
  _conflictFiles: [FilePath]
                 }

makeLenses ''Diff

compareDb :: Database -> Database -> (Database, Diff)
compareDb (Database lrid _ ovis ofis) (Database _ _ nvis nfis) =
  undefined
  where
    added = Map.difference nvis ovis
    nonAdded = Map.intersection nvis ovis
    keepSame = Map.filterWithKey (\k _ -> nvis ! k == ovis ! k) nonAdded
    changed = Map.filterWithKey (\k _ -> nvis ! k /= ovis ! k) nonAdded


server :: Handle -> Handle -> FilePath -> IO ()
server r w dir = do
  db <- syncLocalDb dir
  syncDbToDisk dir db
  sendDbToClient db w
  line <- hGetLine r
  -- maybe turn to client mode
  hPutStrLn w $ "You said " ++ line

client :: Bool -> Handle -> Handle -> FilePath -> IO ()
client turn r w dir = do
  db <- syncLocalDb dir
  line <- hGetLine r
  hPutStrLn stderr $ "The server send database " ++ show line
  let serverDb = (read line) :: Database
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

