import System.IO (isEOF)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad
import Prelude hiding ((.))

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

yieldOne :: Monad m => Producer String m ()
yieldOne = yield "Hello"

yieldTwo :: Monad m => Producer String m ()
yieldTwo = do
  yield "Hello"
  yield "CS240H"

class Category cat where
  (.) :: cat b c -> cat a b  -> cat a c
  id  :: cat a a

(>>>) :: Category cat => cat a b -> cat b c -> cat a c
(>>>) = flip (.)

instance Category (->) where
  -- (.) :: (b -> c) -> (a -> b) -> (a -> c)
  (g . f) x = g (f x)
  id x = x

(>=>) :: Monad m
      => (a -> Producer o m b)
      -> (b -> Producer o m c)
      -> (a -> Producer o m c)
(f >=> g) x = f x >>= g

(~>)  :: (a -> Producer b IO ())
      -> (b -> Producer c IO ())
      -> (a -> Producer c IO ())
(f ~> g) x = for (f x) g

main = (isEOF >>= \eof ->
        if eof
        then return ()
        else (getLine >>= \str ->
                 putStrLn str  >> main))

