{-# LANGUAGE TemplateHaskell #-}

module Trahs where

import           Codec.Digest.SHA
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy     as L
import           Data.Int
import           Data.List
import qualified Data.Map.Strict          as Map
--import qualified Data.Set          as Set
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
data FileInfo = Hash String L.ByteString | Deleted deriving (Show, Read, Eq)
type FileInfos = Map.Map FilePath FileInfo

data VersionInfo = VersionInfo {
    _replicaId :: Int64
  , _versionId :: Int64
  , _fileInfo :: FileInfo
  } deriving (Show, Read, Eq)
makeLenses ''VersionInfo
type VersionInfos = Map.Map FilePath (Map.Map Int64 VersionInfo)

data Database = Database {
    _localReplicaId :: Int64
  , _localVersionId    :: Int64
  , _versionInfos    :: VersionInfos
  } deriving (Show, Read, Eq)
makeLenses ''Database

-- sync local db
localDatabase :: Directory -> IO Database
localDatabase dir = do files <- getDirectoryContents dir
                       increaseVersion $ maybe initDB (fmap read . readFile . (dir </>)) $ find (== dbFile) files
  where
    increaseVersion = fmap (over localVersion (+1))
    generateReplicaId = getStdRandom $ randomR (minBound:: Int64, maxBound :: Int64)
    initDB = generateReplicaId >>= (\rid -> return $ Database rid 0 Map.empty)

localFileInfo :: Directory -> IO FileInfos
localFileInfo dir = do
  files <- getDirectoryContents dir
  watchFile <- filterM (\f -> isFile (dir </> f) >>= return . (&& f /= dbFile)) files
  mapM (\f -> (\h c -> (f, (Hash h c))) <$> fileHash (dir </> f) <*> L.readFile (dir </> f)) watchFile
  where
    isFile f = isRegularFile <$> getSymbolicLinkStatus f
    fileHash path = showBSasHex <$> (hash SHA256 <$> L.readFile path)

mergeInfo :: Database -> FileInfos -> Database
mergeInfo (Database rid v vis) nfis =
  Database rid v mergedVis
  where
    allFiles = Set.union (Map.keysSet vis) (Map.keysSet nfis)
    merge :: Maybe FileInfo -> Maybe FileInfo -> FileInfo
    merge Nothing _ = Deleted
    merge (Just x) Nothing = x
    merge (Just x) (Just y) = case (x, y) of
      (Deleted, Deleted) -> Deleted
      (Deleted, _) -> undefined

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
  _addedFiles:: [FilePath],
  _updatedFiles:: [FilePath],
  _deleteFiles:: [FilePath],
  _conflictFiles:: [FilePath]
                 }

makeLenses ''Diff

data ChangeStatus = Added | Same | Updated | Deleted | Conflict deriving (Show, Eq)

compareDb :: Database -> Database -> (Database, Diff)
compareDb (Database lrid lv ovis ofis) (Database _ _ nvis nfis) =
  (Database lrid lv )
  where
    expectOther = filter (\v -> v^.replicaId /= lrid)
    findLocal = find (\v -> v^.replicaId == lrid)
    statusForReplica :: Maybe VersionInfo -> Maybe VersionInfo -> ChangeStatus
    statusForReplica (Just _) Nothing = Added
    statusForReplica (Just (VersionInfo _ v1 d1)) (Just (VersionInfo _ v2 d2))
      | v1 == v2 = Same
      | v1 /= v2 && d1 = Deleted
      | v1 /= v2 && not d1 = Updated
    changeStatus :: [VersionInfo] -> [VersionInfo] -> ChangeStatus
    changeStatus nvi ovi = let
      localStatus = statusForReplica (findLocal nvi) (findLocal ovi)
      oviMap = Map.fromList (map (\v -> (v^.replicaId, v)) ovi)
      nviMap = Map.fromList (map (\v -> (v^.replicaId, v)) nvi)
      otherStatus = nub $ map (\rid -> statusForReplica (nviMap Map.!? rid) (oviMap Map.!? rid)) (nub $ map (\v -> v^. replicaId) (nvi ++ ovi))
      in case (localStatus, otherStatus) of
           (Added, []) -> Added
           (Same, []) -> Same
           (Same, [x]) -> x
           (Updated, []) -> Updated
           (Updated, [Same]) -> Updated
           (Updated, [Deleted]) -> Updated
           (Updated, [Updated]) -> Conflict
           (Deleted, [Updated]) -> Updated
           (Deleted, [x]) -> Deleted
           e -> error $ "not corver part" ++ (show e)
    calcVersionInfo :: FilePath -> ChangeStatus -> [VersionInfo] -> FileInfo -> [VersionInfo] -> FileInfo -> [(f, [VersionInfo])]
    calcVersionInfo f Added n _ _ _ = (f, n)
    calcVersionInfo f Same n _ _ _ = (f, n)
    calcVersionInfo f Updated n _ _ _ = (f, n)
    calcVersionInfo f Deleted n _ _ _ = (f, n)
    calcVersionInfo f Conflict n (FileInfo nfi) o (FileInfo ofi) = let
      nLocal = fromJust $ findLocal nvis
      oLocal = fromJust $ findLocal ovis
      in
      [(f ++ "#" ++ nfi ++ (nLocal^.replicaId), (VersoinInfo lrid lv nLocal) : (expectOther nvis)), (f ++ "#" ++ ofi ++ (oLocal^.replicaId), (VersoinInfo lrid lv oLocal) : (expectOther ovis))]
    changes = map (\k -> changeStatus (nvis Map.! k) (ovis Map.! k)) (Set.union (Map.keySet nvis) (Map.keySet ovis))

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

