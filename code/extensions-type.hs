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
  
main :: IO ()
main = do
  x <- (getVal :: IO Int)
  putStrLn $ show x
