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

main :: IO ()
main = runStateT go 0 >>= print
  where go = do xplusplus >>= lift . print
                xplusplus >>= lift . print
        xplusplus = do n <- get; put (n+1); return n
