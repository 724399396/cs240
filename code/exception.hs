{-# LANGUAGE ExistentialQuantification #-}
import Data.Typeable

data SomeException = forall e. Exception e => SomeException e
  deriving Typeable

instance Show SomeException where
  show (SomeException e) = show e

class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  toException = SomeException
  fromException :: SomeException -> Maybe e
  fromException (SomeException e) = cast e
