{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class Class a b | a -> b
-- instance (Class a b) => Class [a] Bool  -- bad: 2 * a > 1 * a
-- instance (Class a b)=> Class [a] Bool -- bad: 1*a 1*b > 1*a 0*b

-- instance (Class a Int) => Class a Integer -- bad: 2 >= 2

instance Class a (Maybe a)   -- ok: a "converd" by left
-- instance Class Int (Maybe b) -- bad: b not convered
-- instance Class a (Either a b) -- bad: b not convered
