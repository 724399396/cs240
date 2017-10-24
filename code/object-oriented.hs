{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

import Data.IORef
import Control.Monad.Fix

data HNil = HNil deriving Show
data (:*:) h t = h :*: !t deriving Show
infixr 9 :*:

class Select k h v | k h -> v where
  (.!) :: h -> k -> v
instance Select k ((k, v) :*: t) v where
  (.!) ((_, v) :*: _) _ = v
instance (Select k h v) => Select k (kv' :*: h) v where
  (.!) (kv' :*: h) k = h .! k


returnIO :: a -> IO a
returnIO = return

data GetVal = GetVal deriving Show
data SetVal = SetVal deriving Show
data ClearVal = ClearVal deriving Show

mkVal n self = do
  val <- newIORef (n :: Int)
  returnIO $ (GetVal, readIORef val)
           :*: (SetVal, writeIORef val)
           :*: (ClearVal, self .! SetVal $ 0)
           :*: HNil

test = do
  x <- mfix $ mkVal 7
  x .! GetVal >>= print
  x .! ClearVal
  x .! GetVal >>= print

mkConstVal n self = do
  super <- mkVal n self
  returnIO $ (SetVal, const $ return ())
           :*: super

test2 = do
  x <- mfix $ mkConstVal 7
  x .! GetVal >>= print
  x .! ClearVal
  x .! GetVal >>= print
