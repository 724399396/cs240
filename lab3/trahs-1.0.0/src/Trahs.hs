{-# LANGUAGE TemplateHaskell #-}

module Trahs (
  trahs
, generateReplicaId
, FileInfo(..)
, isFileChange
  ) where

import           Codec.Digest.SHA
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Lazy     as L
import           Data.Int
import           Data.List
import qualified Data.Map.Strict          as Map
import           Data.Time.Clock
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Posix.Types
import           System.PosixCompat.Files
import           System.Process
import           System.Random

trassh :: String
trassh = "ssh -CTaxq @ ./trahs --server"

dbFile :: FilePath
dbFile = ".trahs.db"

data FileInfo = FileInfo {
    _replicaId  :: Int64
  , _versionId  :: Int64
  , _modifyTime :: UTCTime
  , _fileSize   :: Integer
  , _hashValue  :: String
  , _fileName   :: String
  } deriving (Show, Read)

makeLenses ''FileInfo

type History = Map.Map FilePath [FileInfo]

data DataBase = DataBase {
    _clientReplicaId :: Int64
  , _localVersionId  :: Int64
  , _history         :: History
  } deriving (Show, Read)

makeLenses ''DataBase

generateReplicaId :: IO Int64
generateReplicaId =
  getStdRandom $ randomR (minBound:: Int64, maxBound :: Int64)

isFile :: FilePath -> IO Bool
isFile f = isRegularFile <$> getSymbolicLinkStatus f

hashFile :: FilePath -> IO String
hashFile path = showBSasHex <$> (hash SHA256 <$> L.readFile path)

generateFileInfo :: DataBase -> FilePath -> FilePath -> IO FileInfo
generateFileInfo db dir f = let fullFile = dir </> f
                            in do time <- getModificationTime fullFile
                                  size <- getFileSize fullFile
                                  hash <- hashFile fullFile
                                  return $ FileInfo (db^.clientReplicaId) (db^.localVersionId) time size hash f

isFileChange :: FileInfo -> FileInfo -> Bool
isFileChange (FileInfo nId _ nTime nSize nHash _) (FileInfo oId _ oTime oSize oHash _) = nId == oId && (oTime /= nTime || oSize /= nSize || oHash /= nHash)

mergeFileInfo :: FileInfo -> History -> History
mergeFileInfo info history =
  Map.insertWith (\_ olds -> map (\old -> if (isFileChange info old) then info else old) olds) (info^.fileName) [info] history

updateLocalDb :: FilePath -> IO DataBase
updateLocalDb dir = do
  files <- getDirectoryContents dir
  db <- maybe (generateReplicaId >>= (\rid -> return $ DataBase rid 1 Map.empty)) (fmap (over localVersionId (+1) .read) . readFile) $ find (== dbFile) files
  watchFile <- filterM (\f -> isFile f >>= return . (&& f /= dbFile)) files
  nowFileInfos <- mapM (generateFileInfo db dir) watchFile
  return $ over history (\h -> foldr mergeFileInfo h nowFileInfos) db

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

