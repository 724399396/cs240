{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class Generic a rep | a -> rep  where
  from :: a -> rep x
  to :: rep x -> a
