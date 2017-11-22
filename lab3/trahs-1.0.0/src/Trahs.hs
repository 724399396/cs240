module Trahs (
  trahs
, generateReplicaId
  ) where

import           Data.Int
import           Data.List
import qualified Data.Map.Strict    as Map
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix.Types
import           System.Process
import           System.Random
import System.PosixCompat.Files
import Control.Monad

trassh :: String
trassh = "ssh -CTaxq @ ./trahs --server"

dbFile :: FilePath
dbFile = ".trahs.db"

data FileInfo = FileInfo {
    replicaId  :: Int64
  , modifyTime :: EpochTime
  , hash       :: String
  } deriving (Show, Read)

data DataBase = DataBase {
    clientReplicaId :: Int64
  , history         :: Map.Map FilePath FileInfo
  } deriving (Show, Read)

generateReplicaId :: IO Int64
generateReplicaId =
  getStdRandom $ randomR (minBound:: Int64, maxBound :: Int64)

isFile :: FilePath -> IO Bool
isFile f = isRegularFile <$> getSymbolicLinkStatus f

updateLocalDb :: FilePath -> IO DataBase
updateLocalDb dir = do
  files <- getDirectoryContents dir
  db <- maybe (generateReplicaId >>= (\rid -> return $ DataBase rid Map.empty)) (\f -> read <$> readFile f) $ find (== dbFile) files
  let watchFile = filterM (\f -> isFile f >>= return . (&& f /= dbFile)) files
  
  return db

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

