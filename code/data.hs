data T a b = C1 a b | C2 deriving (Typeable, Data)

gfoldl k z (C1 a b) = z C1 `k` a `k` b
gfoldl k z C2       = z C2

