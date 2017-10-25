{-# LANGUAGE ExistentialQuantification #-}
import Data.Typeable

data Step s a = Done | Skip !s | Yield !a !s
data Stream a = forall s. Stream (s -> Step s a) !s

data Showable = forall a. (Show a) => Showable a
instance Show Showable where
  show (Showable a) = "Showable " ++ show a

data Dynamic = forall a. Typeable a => Dynamic a -- opaque type

toDyn :: Typeable a => a -> Dynamic
toDyn = Dynamic

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic a) = cast a
