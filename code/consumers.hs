import Control.Monad.Trans.Class (MonadTrans(lift))

data Consumer a m r
  = Await (a -> Consumer a m r)
  | M       (m (Consumer a m r))
  | Return r

await :: Consumer a m a
await = Await (\a -> Return a)

data Void

type Effect = Consumer Void

runEffect :: Monad m => Effect m r -> m r
runEffect (M      m) = m >>= runEffect
runEffect (Return r) = return r

numbered :: Int -> Consumer String IO r
numbered n = do
  str <- await
  let str' = show n ++ ": " ++ str
  lift (putStrLn str')
  numbered (n+1)

giveString :: Effect IO String
giveString = lift getLine

nl :: Effect IO ()
nl = giveString >~ numbered 0

(>~) :: Monad m
     => Consumer a m b
     -> Consumer b m c
     -> Consumer a m c

awaitTwo :: Monad m => Consumer String m String
awaitTwo = do
  str1 <- await
  str2 <- await
  return (str1 ++ " " ++ str2)

awaitZero :: Monad m => Consumer String m String
awaitZero = return "Some string"
