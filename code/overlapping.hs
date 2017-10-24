{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

class MyShow a where
  myshow :: a -> String

instance MyShow a => MyShow [a] where
  myshow xs = concatMap myshow xs

showHelp :: MyShow a => [a] -> String
showHelp xs = myshow xs

data T = MkT
instance MyShow T where
  myshow x = "Used generic instance"

instance MyShow [T] where
  myshow xs = "Used more specific instance"

main = do { print (myshow [MkT]); print (showHelp [MkT])}
