{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}

import Control.Monad.Fix

class MyClass t where
  myTypeName :: t -> String
  myDefaultValue :: t

instance MyClass Int where
  myTypeName _ = "Int"
  myDefaultValue = 0

getVal :: (MyClass t) => IO t
getVal = mfix $ \t -> do
  putStrLn $ "Caller wants type " ++ myTypeName t
  return myDefaultValue

newtype Identity a = Identity { runIdentity :: a } deriving (Functor)

instance Applicative Identity where
  pure = return
  af <*> ax = Identity $ (runIdentity af) (runIdentity ax)

instance Monad Identity where
  return = Identity
  m >>= k = k (runIdentity m)

main :: IO ()
main = do
  x <- (getVal :: IO Int)
  putStrLn $ show x
