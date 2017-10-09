{-# LANGUAGE DeriveDataTypeable #-}
import Prelude
import Control.Exception
import Data.Typeable
import Control.Concurrent hiding (modifyMVar, Chan)
import Data.Time.Clock
import Control.Monad

data MyError = MyError String deriving (Show, Typeable)
instance Exception MyError

catcher :: IO a -> IO (Maybe a)
catcher action = fmap Just  action `catch` handler
  where handler (MyError msg) = do putStrLn msg; return Nothing

pureCatcher :: a -> IO (Maybe a)
pureCatcher a = (a `seq` return (Just a))
                `catch` \(SomeException _) -> return Nothing

seqList :: [a] -> b -> b
seqList [] b = b
seqList (x:xs) b = x `seq` seqList xs b

data TimeOut = TimeOut UTCTime deriving (Eq, Show, Typeable)
instance Exception TimeOut

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action = do
  -- Create unique exception val(for nested timeouts)
  expired <- fmap TimeOut getCurrentTime
  ptid <- myThreadId
  let child = do threadDelay usec
                 throwTo ptid expired
      parent = do ctid <- forkIO child
                  result <- action
                  killThread ctid
                  return $ Just result
  catchJust (\e -> if e == expired then Just e else Nothing)
            parent
            (\_ -> return Nothing)

modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m action = mask $ \unmask -> do
  v0 <- takeMVar m -- automatically unmasked while waiting
  (v, r) <- unmask (action v0) `onException` putMVar m v0
  putMVar m v
  return r

wrap :: IO a -> IO a
wrap action = do
  mv <- newEmptyMVar
  mask $ \unmask -> do
    tid <- forkIO $ (unmask action >>= putMVar mv) `catch`
                    \e@(SomeException _) -> putMVar mv (throw e)
    let loop = takeMVar mv `catch` \e@(SomeException _) ->
               throwTo tid e >> loop
    loop

type Mutex = MVar ThreadId

mutex_create :: IO Mutex
mutex_create = newEmptyMVar

mutex_lock, mutex_unlock :: Mutex -> IO ()
mutex_lock mv = myThreadId >>= putMVar mv

mutex_unlock mv = do mytid <- myThreadId
                     lockTid <- tryTakeMVar mv
                     unless (lockTid == Just mytid) $
                       error "mutex_unlock"

mutex_synchronize :: Mutex -> IO a -> IO a
mutex_synchronize mv action =
  bracket (mutex_lock mv) (\_ -> mutex_unlock mv)
    (\_ -> action)

data Cond = Cond (MVar [MVar ()])

cond_create :: IO Cond
cond_create = liftM Cond $ newMVar []

cond_wait :: Mutex -> Cond -> IO ()
cond_wait m (Cond waiters) = do
  me <- newEmptyMVar
  modifyMVar_ waiters $ \others -> return $ others ++ [me]
  mutex_unlock m
  takeMVar me `finally` mutex_lock m

cond_signal, cond_broadcast :: Cond -> IO ()
cond_signal (Cond waiters) = modifyMVar_ waiters wakeone
  where wakeone [] = return []
        wakeone (w:ws) = putMVar w () >> return ws

cond_broadcast (Cond waiters) = modifyMVar_ waiters wakeall
  where wakeall ws = do mapM_ (flip putMVar ()) ws
                        return []

data Item a = Item a (Stream a)
type Stream a = MVar (Item a)
data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))

newChan :: IO (Chan a)
newChan = do
  empty <- newEmptyMVar
  liftM2 Chan (newMVar empty) (newMVar empty)

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ w) a = do
  empty <- newEmptyMVar
  modifyMVar_ w $ \oldEmpty -> do
    putMVar oldEmpty (Item a empty)
    return empty

readChan :: Chan a -> IO a
readChan (Chan r _) =
  modifyMVar r $ \full -> do
  (Item a newFull) <- takeMVar full
  return (newFull, a)

main :: IO ()
main = undefined

withClient
