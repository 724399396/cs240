{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
class MyShow a where myShow :: a -> String

data MyType

instance MyShow MyType where myShow = genericMyShow

class MetaData d m | d -> m, m -> d where -- not what GHC does
  fromData :: d -> m
  toData :: m -> d

class MetaMyShow a where metaMyShow :: a -> String

genericMyShow :: (MetaData d m, MetaMyShow m) => d -> String
genericMyShow = metaMyShow . fromData
