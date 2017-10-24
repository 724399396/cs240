{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
data HFalse = HFalse deriving Show
data HTrue = HTrue deriving Show

class HNot a b | a -> b where hNot :: a -> b
instance HNot HFalse HTrue where hNot _ = HTrue
instance HNot HTrue HFalse where hNot _ = HFalse

-- problem :: TypeEq a a HTrue not more special than TypeEq a b HFalse
class TypeEq a b c | a b -> c where typeEq :: a -> b -> c
-- instance TypeEq a b HTrue where typeEq _ _ = HTrue
-- instance TypeEq a b HFalse where typeEq _ _ = HFalse

class TypeCast a b | a -> b where typeCast :: a -> b

instance TypeCast a a where typeCast = id

instance TypeEq a a HTrue where typeEq _ _ = HTrue
instance (TypeCast HFalse c) => TypeEq a b c where
  typeEq _ _ = typeCast HFalse
