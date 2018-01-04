{-# LANGUAGE TemplateHaskell #-}

module Trahs where

import           Codec.Digest.SHA
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy     as L
import           Data.Int
import           Data.List
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
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
newtype ReplicaId = ReplicaId Int64 deriving (Show, Read, Eq, Ord, Bounded)
newtype Version = Version Int64 deriving (Show, Read, Eq, Ord)
newtype Hash = Hash String deriving (Show, Read, Eq, Ord)
type VersionInfo = Map.Map (FilePath, ReplicaId) Version
type FileInfo = Map.Map FilePath Hash
type FileContent = Map.Map FilePath L.ByteString

data Database = Database {
    _replicaId     :: ReplicaId
  , _versionId     :: Version
  , _dbVersionInfo :: VersionInfo
  , _dbFileInfo    :: FileInfo
  } deriving (Show, Read, Eq)

makeLenses ''Database

data DbWithContent = DbWithContent Database FileContent deriving (Show, Read)

-- sync local db
localDatabase :: Directory -> IO Database
localDatabase dir = do files <- getDirectoryContents dir
                       increaseVersion $ maybe initDB (fmap read . readFile . (dir </>)) $ find (== dbFile) files
  where
    increaseVersion = fmap (over versionId (\(Version v) -> Version $ v+1))
    generateReplicaId = getStdRandom $ randomR (minBound:: Int64, maxBound :: Int64)
    initDB = generateReplicaId >>= (\rid -> return $ Database (ReplicaId rid) (Version 0) Map.empty Map.empty)

localFileInfo :: Directory -> IO (FileInfo, FileContent)
localFileInfo dir = do
  files <- getDirectoryContents dir
  watchFile <- filterM (\f -> isFile (dir </> f) >>= return . (&& f /= dbFile)) files
  fc <- mapM (\f -> L.readFile (dir </> f) >>= return . (,) f) watchFile
  return $ (,) (Map.fromList $ map (\(f,c) -> (f, Hash $ fileHash c)) fc)
               (Map.fromList $ fc)
  where
    isFile f = isRegularFile <$> getSymbolicLinkStatus f
    fileHash = showBSasHex . hash SHA256

mergeLocalInfo :: Database -> FileInfo -> Database
mergeLocalInfo (Database rid vid vis fis) nfis =
  Database rid vid finalVis nfis
  where
    allFs = (Map.keys fis) ++ (Map.keys nfis)
    changeFs = filter (\f -> fis Map.!? f /= nfis Map.!? f) allFs
    finalVis = foldl' (\m f -> Map.insert (f,rid) vid m) vis changeFs

syncLocalDb :: FilePath -> IO (Database, FileContent)
syncLocalDb dir = do db <- localDatabase dir
                     (fi, fc) <- localFileInfo dir
                     return $ (mergeLocalInfo db fi, fc)

syncDbToDisk :: FilePath -> Database -> IO ()
syncDbToDisk dir db = writeToExistFile dir dbFile (\h -> hPutStr h (show db))

sendDbToClient :: DbWithContent -> Handle -> IO ()
sendDbToClient db h = hPutStrLn h (show db)

data ChangeStatus = Same | Update | Delete | Conflict deriving (Show, Eq, Ord)

compareDb :: Database -> Database -> Map.Map ChangeStatus (Set.Set FilePath)
compareDb (Database lrid _ lvis lfis) (Database orid _ ovis ofis) =
   foldl (\acc (f, status) -> Map.insertWith Set.union status (Set.singleton f) acc) Map.empty allMergeInfo
  where
    allMergeInfo :: Set.Set (FilePath, ChangeStatus)
    allMergeInfo = Set.map (\f -> (f, merge f (lfis Map.!? f) (ofis Map.!? f))) (Map.keysSet lfis `Set.union` Map.keysSet ofis)
    findWithDefaultZero = Map.findWithDefault (Version 0)
    merge :: FilePath -> Maybe Hash -> Maybe Hash -> ChangeStatus
    merge f Nothing (Just _) = let lvFo = findWithDefaultZero (f, orid) lvis
                                   ovFo = findWithDefaultZero (f, orid) ovis
                               in if (ovFo > lvFo)
                                  then Update
                                  else Same
    merge f (Just _) Nothing = let lvFi = findWithDefaultZero (f, lrid) lvis
                                   ovFi = findWithDefaultZero (f, lrid) ovis
                               in if (lvFi > ovFi)
                                  then Same
                                  else Delete
    merge f (Just c1) (Just c2)
      | c1 == c2 = Same
      | c1 /= c2 = let lvFo = findWithDefaultZero (f, orid) lvis
                       ovFo = findWithDefaultZero (f, orid) ovis
                       lvFl = findWithDefaultZero (f, lrid) lvis
                       ovFl = findWithDefaultZero (f, lrid) ovis
                       oCompare = ovFo `compare` lvFo
                       lCompare = ovFl `compare` lvFl
                   in case (oCompare, lCompare) of
                        (EQ, _)  -> Same
                        (GT, EQ) -> Update
                        (GT, LT) -> Conflict
                        _        -> error "not impossible"
    merge _ _ _ = error "not impossible"

conflictFileName :: FilePath -> ReplicaId -> Version -> FilePath
conflictFileName f (ReplicaId r) (Version v) = (f ++ "#" ++ (show r) ++ "." ++ (show v))

mergeDb :: Database -> Database -> Map.Map ChangeStatus (Set.Set FilePath) -> Database
mergeDb (Database lrid lvid lvis lfis) (Database orid ovid ovis ofis) changeStatus =
  Database lrid lvid updatedVis updatedFis
  where
    updateVis :: VersionInfo -> ChangeStatus -> Set.Set FilePath -> VersionInfo
    updateVis vi Same fs = foldl' (\vi' f' -> maybe vi' (\v' -> Map.insert (f', orid) v' vi') (ovis Map.!? (f', orid))) vi fs
    updateVis vi Update fs = foldl' (\vi' f' -> Map.insert (f', orid) (ovis Map.! (f', orid)) vi') vi fs
    updateVis vi Delete fs = foldl' (\vi' f' -> Map.insert (f', orid) (ovis Map.! (f', orid)) vi') vi fs
    updateVis vi Conflict fs = let
      updateOrigined = foldl' (\vi' f' -> Map.insert (f', orid) (ovis Map.! (f', orid)) vi') vi fs
      insertConflict = foldl' (\vi' f' -> Map.insert (conflictFileName f' lrid lvid, lrid) lvid $ Map.insert (conflictFileName f' orid ovid, lrid) lvid vi') updateOrigined fs
      in
        insertConflict
    updatedVis = Map.foldlWithKey updateVis lvis changeStatus
    updateFis :: FileInfo -> ChangeStatus -> Set.Set FilePath -> FileInfo
    updateFis fi Same _ = fi
    updateFis fi Update fs = foldl' (\fi' f' -> Map.insert f' (ofis Map.! f') fi') fi fs
    updateFis fi Delete fs = Map.withoutKeys fi fs
    updateFis fi Conflict fs = let
      removeOrigined = Map.withoutKeys fi fs
      insertConflict = foldl' (\fi' f' -> Map.insert (conflictFileName f' lrid lvid) (lfis Map.! f') $ Map.insert (conflictFileName f' orid ovid) (ofis Map.! f') fi') removeOrigined fs
      in
        insertConflict
    updatedFis = Map.foldlWithKey updateFis lfis changeStatus

writeToExistFile :: Directory -> FilePath -> (Handle -> IO ()) -> IO ()
writeToExistFile dir fileName act =
  withTempFile dir fileName $ (\nPath h ->
                                  do act h
                                     renameFile nPath (dir </> fileName))

syncFileFromServer :: FileContent -> Directory -> Database -> Database -> Map.Map ChangeStatus (Set.Set FilePath) -> IO ()
syncFileFromServer fc dir (Database lrid lvid _ _) (Database orid ovid _ _) changeStatus =
  mapM_ dispatch (Map.assocs changeStatus)
  where
    dispatch :: (ChangeStatus, Set.Set FilePath) -> IO ()
    dispatch (Same,_)      = return ()
    dispatch (Delete,fs)   = mapM_ deleteFile $ Set.toList fs
    dispatch (Update,fs)   = mapM_ downloadFile $ Set.toList fs
    dispatch (Conflict,fs) = mapM_ conflictFile $ Set.toList fs
    readFromServer :: FilePath -> L.ByteString
    readFromServer fileName = fc Map.! fileName
    deleteFile :: FilePath -> IO ()
    deleteFile fileName = do
      hPutStrLn stderr $ "fetching \"" ++ fileName ++ "\""
      removeFile fileName
    downloadFile :: FilePath -> IO ()
    downloadFile fileName = do
      hPutStrLn stderr $ "fetching \"" ++ fileName ++ "\""
      writeToExistFile dir fileName (\h -> L.hPutStr h (readFromServer fileName))
    conflictFile :: FilePath -> IO ()
    conflictFile fileName = do
      lCon <- L.readFile (dir </> fileName)
      let oCon = readFromServer fileName
      L.writeFile (dir </> (conflictFileName fileName lrid lvid)) lCon
      seq oCon $ L.writeFile (dir </> (conflictFileName fileName orid ovid)) oCon
      removeFile (dir </> fileName)
      hPutStrLn stderr $ "conflicting \"" ++ fileName ++ "\""

server :: Handle -> Handle -> FilePath -> IO ()
server r w dir = do
  (db, fc) <- syncLocalDb dir
  syncDbToDisk dir db
  sendDbToClient (DbWithContent db fc) w
  line <- hGetLine r
  if (read line)
    then do hPutStrLn stderr "=== switching from client to server ==="
            client False r w dir
    else return ()

client :: Bool -> Handle -> Handle -> FilePath -> IO ()
client turn r w dir = do
  (db, _) <- syncLocalDb dir
  line <- hGetLine r
  let (DbWithContent serverDb fc) = read line
      compareResult = compareDb db serverDb
      mergeResult = mergeDb db serverDb compareResult
  syncDbToDisk dir mergeResult
  syncFileFromServer fc dir mergeResult serverDb compareResult
  hPutStrLn w (show turn)
  if turn
    then do hPutStrLn stderr $ "=== switch from client to server ==="
            server r w dir
    else return ()

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

