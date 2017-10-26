{-# LANGUAGE DeriveGeneric, TypeFamilies, TypeOperators,
    FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

import GHC.Generics

data X = X
undef2 :: mi c f p -> f p
undef2 _ = undefined

-- A unit type has one constructor and no arguments
data T1 = C1 deriving (Show, Generic)
