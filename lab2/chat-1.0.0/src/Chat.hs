-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Network.Socket
import           System.Environment
import           System.IO

tcpListenOn :: IO Socket
tcpListenOn = do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }
  port <- getEnv "CHAT_SERVER_PORT"
  addr:_ <- getAddrInfo (Just hints) (Just "0.0.0.0") (Just port)
  socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

chooseUserName :: Num a => MVar a -> IO a
chooseUserName mi = modifyMVar mi $ \prev -> return (prev+1, prev)

receiveConnect :: Socket -> MVar [Handle] -> IO ()
receiveConnect s handles = bracket (socketToHandle s ReadWriteMode) hClose
                           $ \h -> do modifyMVar_ handles (\hs -> return $ h:hs)
                                      forkIO $ forever $ do inp <- hGetLine h
                                                            hs <- takeMVar handles
                                                            let others = filter (/= h) hs
                                                            forM_ others (flip hPutStrLn inp)
                                      return ()


-- | Chat server entry point.
chat :: IO ()
chat = return ()

