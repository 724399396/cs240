import System.IO (isEOF)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad

data Producer a m r
  = Yield a (Producer a m r)
  | M    (m (Producer a m r))
  | Return r

yield :: a -> Producer a m ()
yield a = Yield a (Return ())

instance Monad m => Functor (Producer a m) where
  fmap f ma = ma >>= return . f

instance Monad m => Applicative (Producer a m) where
  pure = return
  mf <*> ma = do f <- mf
                 a <- ma
                 return $ f a

instance Monad m => Monad (Producer a m) where
--  return :: Monad m => r -> Producer a m r
    return r = Return r
-- (>>=) :: Monad m
--       => Producer a m r -> (r -> Producer a m s) -> Producer a m s
    (Yield a p) >>= return' = Yield a (p >>= return')
    (M       m) >>= return' = M (m >>= \p -> return (p >>= return'))
    (Return  r) >>= return' = return' r

instance MonadTrans (Producer a) where
-- lift :: Monad m => m r -> Producer a m r
   lift m = M (liftM Return m)

for :: Monad m
    => Producer a m ()
    -> (a -> Producer b m ())
    -> Producer b m ()
for (Yield a p) yield' = yield' a >> for p yield'
for (M       m) yield' = M (m >>= \p -> return (for p yield'))
for (Return  r) _      = Return r

data Void

type Effect = Producer Void

runEffect :: Monad m => Effect m r -> m r
runEffect (M      m) = m >>= runEffect
runEffect (Return r) = return r

stdinLn =
    M (isEOF >>= \eof -> return $
        if eof
        then Return ()
        else M (getLine >>= \str ->
            return $ Yield str stdinLn ) )

useString str = M (putStrLn str >>= \r -> return (Return r))

echo = M (isEOF >>= \eof -> return $
        if eof
        then Return ()
        else M (getLine >>= \str ->
            return $ M (putStrLn str >>= \r -> return (Return r))  >> echo ))


main = (isEOF >>= \eof ->
        if eof
        then return ()
        else (getLine >>= \str ->
                 putStrLn str  >> main))

