import Pipes
import Pipes.Prelude (stdinLn, stdoutLn)
import Prelude hiding (takeWhile)

takeWhile :: Monad m => (a -> Bool) -> Pipe a a m ()
takeWhile keep = do
  a <- await
  if (keep a)
    then do yield a
            takeWhile keep
    else return ()

map :: Monad m => (a -> b) -> Pipe a b m ()
map f = for cat (yield . f)

grep :: Monad m => String -> Pipe String String m r
grep str = Pipes.filter (str `inInfixOf`)
