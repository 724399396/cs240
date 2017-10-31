import Control.Monad (forever)
import Prelude hiding (take)
import Control.Monad.Trans.Class (MonadTrans(lift))

data Pipe a b m r
  = Await (a -> Pipe a b m r)
  | Yield  b   (Pipe a b m r)
  | M     (m   (Pipe a b m r))
  | Return r

await :: Pipe a b IO a
await = Await (\a -> Return a)

yield :: b -> Pipe a b IO ()
yield b = Yield b (Return ())

take :: Int -> Pipe a a IO ()
take n | n <= 0 = lift (putStrLn "You shall not pass!")
       | otherwise = do a <- await
                        yield a
                        take (n - 1)

cat :: Pipe a a IO r
cat = forever $ do
  a <- await
  yield a
