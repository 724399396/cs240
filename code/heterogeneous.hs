{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

data HNil = HNil deriving Show
data (:*:) h t = h :*: !t deriving Show
infixr 9 :*:

data A = A deriving Show
data B = B deriving Show
data C = C deriving Show

foo = (A, "Hello") :*: (B, 7) :*: (C, 3.0) :*: HNil

class Select k h v | k h -> v where
  (.!) :: h -> k -> v
instance Select k ((k, v) :*: t) v where
  (.!) ((_, v) :*: _) _ = v
instance (Select k h v) => Select k (kv' :*: h) v where
  (.!) (kv' :*: h) k = h .! k
