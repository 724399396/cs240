{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

import GHC.Generics

-- | Unit: used for constructors without arguments
data U1 p = U1

-- | Meta-information (constructor names, ect.)
newtype M1 i c f p = M1 { unM1 :: f p }

-- | Three flavors of meta-informating for variable i
data D; type D1 = M1 D -- c instance of Datatype, f is C1 (or :+:)
data C; type C1 = M1 C -- c instance of Constructor, f is S1 (or :*:)
data S; type S1 = M1 S -- c instance of Selector, f is U1 (or Rec0)

class Datatype d where
  datatypeName :: t d (f :: * -> *) a -> String
  moduleName   :: t d (f :: * -> *) a -> String
class Constructor c where
  conName :: t c (f :: * -> *) a -> String
class Selector s where
  selName :: t s (f :: * -> *) a -> String

data T2 = C2 { t2a :: Bool } deriving (Show, Generic)
data T3 = C3 { t3a :: Bool, t3b :: Bool } deriving (Show, Generic)
