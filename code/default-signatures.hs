{-# LANGUAGE DefaultSignatures #-}

class MyShow a where
  myShow :: a -> String
  default myShow :: (MetaData a m, MetaMyShow m) -> a -> String
  myShow = genericMyShow
