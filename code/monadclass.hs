{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

data StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance Functor (StateT s m) where
  fmap = undefined

instance Applicative (StateT s m) where
  pure = return
  (<*>) = undefined

instance Monad (StateT s m) where
  return = undefined
  (>>=) = undefined

class (Monad m) => MonadState s m where
  get :: m s
  put :: s -> m ()

instance (Monad m) => MonadState s (StateT s m) where
  get = StateT $ \s -> return (s, s)
  put s = StateT $ \_ -> return ((), s)

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadState s m) => MonadState s (ReaderT r m) where
  get = lift get
  put = lift . put
