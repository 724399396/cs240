{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Monad
import Control.Monad.Trans.Class

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id

-- undecidable: assertion Monad (t m) no smaller than head
instance (MonadTrans t, MonadIO m, Monad (t m)) =>
  MonadIO (t m) where liftIO = lift . liftIO
