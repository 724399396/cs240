import Control.Concurrent
import Control.Exception
import Control.Monad
import Criterion.Main

pingpong :: Bool -> Int -> IO ()
pingpong v n = do
  mvc <- newEmptyMVar
  mvp <- newEmptyMVar
  let parent n | n > 0 = do when v $ putStr $ " " ++ show n
                            putMVar mvc n
                            takeMVar mvp >>= parent
               | otherwise = return ()
      child = do n <- takeMVar mvc
                 putMVar mvp (n - 1)
                 child
  tid <- forkOS child
  parent n `finally` killThread tid
  when v $ putStrLn ""

main :: IO ()
main = defaultMain [
  bench "thread switch test" $ whnfIO mybench]
  where mybench = pingpong False 10000
