import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM

type Account = MVar Double

printMV :: (Show a) => MVar a -> IO ()
printMV mv = withMVar mv print

transfer :: Double -> Account -> Account -> IO ()
transfer amount from to = do
  let tryTransfer = modifyMVar from $ \ bf -> do
        when (bf < amount) $ fail "not enough money"
        mbt <- tryTakeMVar to
        case mbt of
          Just bt -> do putMVar to $! bt + amount
                        return (bf - amount, True)
          Nothing -> return (bf, False)
  ok <- tryTransfer
  unless ok $ transfer (- amount) to from

type TAccount = TVar Double

transferSTM :: Double -> TAccount -> TAccount -> STM ()
transferSTM amount from to = do
  bf <- readTVar from
  when (amount > bf) retry
  modifyTVar' from (subtract amount)
  modifyTVar' to (+ amount)

transfer2 :: Double -> TAccount -> TAccount -> TAccount -> IO ()
transfer2 amount from1 from2 to =
  atomically $ transferSTM amount from1 to
               `orElse` transferSTM amount from2 to

newAccount :: Double -> STM TAccount
newAccount balance = do
  tv <- newTVar balance
  alwaysSucceeds $ do balance <- readTVar tv
                      when (balance < 0) $ fail "negative balance"
  return tv

bogus :: IO ()
bogus = do
  ac <- atomically $ newAccount 10
  atomically $ modifyTVar ac (subtract 15)

main :: IO ()
main = do
  ac1 <- newTVarIO 10
  ac2 <- newTVarIO 0
  atomically $ transferSTM 1 ac1 ac2
