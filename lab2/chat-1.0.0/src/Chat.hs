-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Network.Socket
import           System.Environment
import           System.IO

type UserName = Int

tcpListenOn :: IO Socket
tcpListenOn = do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }
  port <- getEnv "CHAT_SERVER_PORT"
  addr:_ <- getAddrInfo (Just hints) (Just "0.0.0.0") (Just port)
  s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind s (addrAddress addr)
  listen s 1
  return s

userNameStart :: UserName
userNameStart = 1

chooseUserName :: MVar UserName -> IO UserName
chooseUserName mi = modifyMVar mi $ \prev -> return (prev+1, prev)

receiveConnect :: Socket -> UserName -> MVar [Handle] -> IO ()
receiveConnect s userName handles = flip catch (\(SomeException _) -> return ())
  $ do h <- socketToHandle s ReadWriteMode
       modifyMVar_ handles (return . (h:))
       hs <- takeMVar handles
       forM_ hs (flip hPutStrLn $ show userName ++ " has joined.")
       forever $ do inp <- hGetLine h
                    takeMVar handles >>= mapM_ (flip hPutStrLn $ show userName ++ ": " ++ inp) . filter (/=h)


-- | Chat server entry point.
chat :: IO ()
chat = do
  s <- tcpListenOn
  uNameM <- newMVar userNameStart
  handlesM <- newMVar []
  forever $ do (ns, _) <- accept s
               forkIO $ do uName <- chooseUserName uNameM
                           receiveConnect ns uName handlesM
