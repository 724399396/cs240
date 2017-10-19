{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}

import Control.Concurrent
import System.IO.Unsafe
import Data.IORef
import Control.Exception

class MonadTrans t where
  lift :: Monad m => m a -> t m a

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad f) => Functor (StateT s f) where
  fmap f m = StateT $ \s -> do
    (a, s') <- runStateT m s
    return (f a, s')

instance (Monad m) => Applicative (StateT s m) where
  pure = return
  mf <*> ma = StateT $ \s0 -> do
    (f,s1) <- runStateT mf s0
    (a,s2) <- runStateT ma s1
    return (f a, s2)

instance (Monad m) => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  m >>= k  = StateT $ \s0 -> do
    ~(a, s1) <- runStateT m s0
    runStateT (k a) s1

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a,s)

get :: (Monad m) => StateT s m s
get = StateT $ \s ->
  return (s,s)

put :: (Monad m) => s -> StateT s m ()
put s = StateT $ \_ -> return ((),s)

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO

myprint :: (Show a, MonadIO m) => a -> m ()
myprint a = liftIO $ print $ show a

oneTwo :: (Int, Int)
oneTwo = (fst y, snd x)
  where x = (1, snd y)
        y = (fst x, 2)

nthFib :: Int -> Integer
nthFib n = fibList !! n
  where fibList = 1 : 1 : zipWith (+) fibList (tail fibList)

fix :: (a -> a) -> a
fix f = let x = f x in x

oneTwo' :: (Int, Int)
oneTwo' = (fst y, snd x)
  where (x, y) = fix $ \ ~(x0,y0) -> let x1 = (1, snd y0)
                                         y1 = (fst x1, 2)
                                     in (x1, y1)

nthFib' :: Int -> Integer
nthFib' n = fibList !! n
  where fibList = fix $ \l -> 1 : 1 : zipWith (+) l (tail l)

class Monad m => MonadFix m where
  mfix :: (a -> m a) -> m a

mfib :: (MonadFix m) => Int -> m Integer
mfib n = do
  fibList <- mfix $ \l -> return $ 1 : 1 : zipWith (+) l (tail l)
  return $ fibList !! n

oneTwo'' :: (MonadFix m) => m (Int, Int)
oneTwo'' = do
  (x,y) <- mfix $ \ ~(x0, y0) -> do x1 <- return (1, snd y0)
                                    y1 <- return (fst x0, 2)
                                    return (x1, y1)
  return (fst y, snd x)

data Link a = Link !a !(MVar (Link a))

mkCycle :: IO (MVar (Link Int))
mkCycle = do
  rec l1 <- newMVar $ Link 1 l2
      l2 <- newMVar $ Link 2 l1
  return l1

newtype Identity a = Identity { runIdentity :: a } deriving (Functor)

instance Applicative Identity where
  pure = return
  af <*> ax = Identity $ (runIdentity af) (runIdentity ax)

instance Monad Identity where
  return = Identity
  m >>= k = k (runIdentity m)

instance MonadFix Identity where
  mfix f = let x = f (runIdentity x) in x

weird :: IO String
weird = do
  xxx <- unsafeInterleaveIO $ do putStrLn "Gotcha!"; return []
  return $ 'a':'b':'c':xxx

fixIO :: (a -> IO a) -> IO a
fixIO k = do
  ref <- newIORef (throw NonTermination)
  ans <- unsafeInterleaveIO (readIORef ref)
  result <- k ans
  writeIORef ref result
  return result

mbroken :: (Monad m) => (a -> m a) -> m a
mbroken f = fix (>>= f)

instance MonadFix m => MonadFix (StateT s m) where
  mfix f = StateT $ \s -> mfix $ \ ~(a, _) -> runStateT (f a) s

class Convert a b where convert :: a -> b
instance Convert Int Bool where convert = (/= 0)
instance Convert Int Integer where convert = toInteger
instance (Convert a b) => Convert [a] [b] where
  convert = map convert

instance Convert Int [Char] where convert = show
instance Convert a a where convert a = a

class MyShow a where myShow :: a -> String
instance MyShow Char where my

main :: IO ()
main = runStateT go 0 >>= print
  where go = do xplusplus >>= lift . print
                xplusplus >>= lift . print
        xplusplus = do n <- get; put (n+1); return n
