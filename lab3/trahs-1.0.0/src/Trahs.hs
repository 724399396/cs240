{-# LANGUAGE TemplateHaskell #-}

module Trahs where

import           Codec.Digest.SHA
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy     as L
import           Data.Int
import           Data.List
import qualified Data.Map.Strict          as Map
import qualified Data.Set as Set
import           Data.Maybe               (fromJust, maybeToList)
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
type VersionInfo = Map.Map (FilePath, ReplicaId) Version
type FileInfo = Map.Map FilePath String

data Database = Database {
    _replicaId     :: ReplicaId
  , _versionId     :: Version
  , _dbVersionInfo :: VersionInfo
  , _dbFileInfo    :: FileInfo
  } deriving (Show, Read, Eq)

makeLenses ''Database

-- sync local db
localDatabase :: Directory -> IO Database
localDatabase dir = do files <- getDirectoryContents dir
                       increaseVersion $ maybe initDB (fmap read . readFile . (dir </>)) $ find (== dbFile) files
  where
    increaseVersion = fmap (over versionId (\(Version v) -> Version $ v+1))
    generateReplicaId = getStdRandom $ randomR (minBound:: Int64, maxBound :: Int64)
    initDB = generateReplicaId >>= (\rid -> return $ Database (ReplicaId rid) (Version 0) Map.empty Map.empty)

localFileInfo :: Directory -> IO FileInfo
localFileInfo dir = do
  files <- getDirectoryContents dir
  watchFile <- filterM (\f -> isFile (dir </> f) >>= return . (&& f /= dbFile)) files
  Map.fromList <$> (mapM (\f -> fileHash (dir </> f) >>= \h -> return (f, h)) watchFile)
  where
    isFile f = isRegularFile <$> getSymbolicLinkStatus f
    fileHash path = showBSasHex <$> (hash SHA256 <$> L.readFile path)

mergeLocalInfo :: Database -> FileInfo -> Database
mergeLocalInfo (Database rid vid vis fis) nfis =
  Database rid vid finalVis nfis
  where
    calVersionCurrentReplica :: Maybe (Version, String) -> Maybe String -> Maybe Version
    calVersionCurrentReplica (Just _) Nothing = Just vid
    calVersionCurrentReplica Nothing (Just _) = Just vid
    calVersionCurrentReplica (Just (ovid, c1)) (Just c2) = Just $ if (c1 == c2) then ovid else vid
    calVersionCurrentReplica _ _ = error "not complete calVersionCurrentReplica"

    findFileLocalVersion :: FilePath -> Maybe (Version, String)
    findFileLocalVersion f = (,) <$> vis Map.!? (f, rid) <*> fis Map.!? f

    findFileOtherVersion :: FilePath -> [((FilePath, ReplicaId), Version)]
    findFileOtherVersion f = filter (\((f',rid'), _) -> f == f' && rid /= rid') (Map.assocs vis)

    calVersionInfo :: FilePath -> Maybe String -> [((FilePath,ReplicaId),Version)]
    calVersionInfo f nc = (maybeToList ((,) (f,rid) <$> calVersionCurrentReplica (findFileLocalVersion f) nc)) ++ findFileOtherVersion f
    finalVis :: Map.Map (FilePath, ReplicaId) Version
    finalVis = Map.fromList (join $ map (\f -> calVersionInfo f (nfis Map.!? f)) (nub $ Map.keys nfis ++ Map.keys fis))

syncLocalDb :: FilePath -> IO Database
syncLocalDb dir = do db <- localDatabase dir
                     fi <- localFileInfo dir
                     return $ mergeLocalInfo db fi

syncDbToDisk :: FilePath -> Database -> IO ()
syncDbToDisk dir db =
  do withTempFile dir dbFile $ (\nPath h ->
         do hPutStr h (show db)
            renameFile nPath (dir </> dbFile))

sendDbToClient :: Database -> Handle -> IO ()
sendDbToClient db h = hPutStrLn h (show db)

data ChangeStatus = Same | Update | Delete | Conflict deriving (Show, Eq, Ord)

compareDb :: Database -> Database -> Map.Map ChangeStatus [FilePath]
compareDb (Database lrid _ lvis lfis) (Database orid _ ovis ofis) =
   foldl (\acc (f, status) -> Map.insertWith (++) status [f] acc) Map.empty allMergeInfo
  where
    allMergeInfo :: [(FilePath, ChangeStatus)]
    allMergeInfo = map (\f -> (f, merge f (lfis Map.!? f) (ofis Map.!? f))) (nub $ Map.keys lfis ++ Map.keys ofis)
    findWithDefaultZero = Map.findWithDefault (Version 0)
    merge :: FilePath -> Maybe String -> Maybe String -> ChangeStatus
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
                        (EQ, _) -> Same
                        (GT, EQ) -> Update
                        (GT, LT) -> Conflict
                        _ -> error "not impossible"
    merge _ _ _ = error "not impossible"

mergeDb :: Database -> Database -> Map.Map ChangeStatus [FilePath] -> Database
mergeDb (Database lrid lvid lvis lfis) (Database orid _ ovis ofis) changeStatus =
  Database lrid lvid updatedVis updatedFis
  where
    updateVis :: VersionInfo -> (FilePath, ReplicaId) -> Version -> VersionInfo
    updateVis m (f, inputRid) v | inputRid == orid = Map.insert (f, orid) v m
                                | otherwise        = m
    updatedVis = Map.foldlWithKey updateVis lvis ovis
    updateFis :: FileInfo -> ChangeStatus -> [FilePath] -> FileInfo
    updateFis fi Same _ = fi
    updateFis fi Update fs = foldl' (\fi' f' -> Map.insert f' (ofis Map.! f') fi') fi fs
    updateFis fi Delete fs = Map.withoutKeys fi (Set.fromList fs)
    updateFis fi Conflict fs = let
      removeOrigined = Map.withoutKeys fi (Set.fromList fs)
      insertRConflict = foldl' (\fi' f' -> Map.insert (f' ++ "#" ++ (ofis Map.! f') ++ (show $ ovis Map.! (f',orid))) (ofis Map.! f') fi') removeOrigined fs
      insertLConflict = foldl' (\fi' f' -> Map.insert (f' ++ "#" ++ (lfis Map.! f') ++ (show $ lvis Map.! (f',lrid))) (lfis Map.! f') fi') insertRConflict fs
      in
        insertLConflict
    updatedFis = Map.foldlWithKey updateFis lfis changeStatus

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
      compareResult = compareDb db serverDb
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

