{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable hiding (cast)
import Data.Data hiding (cast)
import Unsafe.Coerce
import Data.Function

pairToStringList :: (Show a, Show b) => (a, b) -> [String]
pairToStringList (a, b) = [show a, show b]

pairToIntList :: (Enum a, Enum b) => (a, b) -> [Int]
pairToIntList (a, b) = [fromEnum a, fromEnum b]

-- pairToList :: (a -> b) -> (a, a) -> [b]
-- pairToList conv (a, b) = [conv a, conv b]
-- pairToList show (True, Just 3) type error

class Function f a b | f a -> b where
  funcall :: f -> a -> b
instance Function (a -> b) a b where
  funcall = id

pairToList :: (Function f a c, Function f b c) =>
  f -> (a, b) -> [c]
pairToList f (a, b) = [funcall f a, funcall f b]

data ShowF = ShowF
instance (Show a) => Function ShowF a [Char] where
  funcall _ = show

data FromEnumF = FromEnumF
instance (Enum a) => Function FromEnumF a Int where
  funcall _ = fromEnum

class TupleFoldr f z t r | f z t -> r where
  tupleFoldr :: f -> z -> t -> r

data MyType = Con1 Int | Con2 String deriving (Typeable, Data)
data MyTyCon a = MyTyCon a deriving (Typeable, Data)

-- class Typeable a where
--  typeOf :: a -> TypeRep -- Note: nover evaluates arguments

-- data TypeRep -- Opaque, but instance of Eq, Ord, Show, Typeable

rtTypeEq :: (Typeable a ,Typeable b) => a -> b -> Bool
rtTypeEq a b = typeOf a == typeOf b

cast :: (Typeable a, Typeable b) => a -> Maybe b
cast a = fix $ \ ~(Just b) -> if typeOf a == typeOf b
                                 then Just $ unsafeCoerce a
                                 else Nothing
