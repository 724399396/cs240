{-# LANGUAGE MagicHash #-}
import GHC.Prim

data FastPoint = FastPoint Double# Double#

fp = FastPoint 2.0## 2.0##

-- Error: can't pass unboxed type to polymorphic function
-- fp' = FastPoint 2.0## (id 2.0##)

-- Error: can't use unboxed type as type parameter
-- notInt :: Maybe Int#
-- notInt = Nothing

infiniteLoop = infiniteLoop :: Char

seqTest1 = infiniteLoop `seq` "Hello" -- loops forever

seqTest2 = str `seq` length str
           where str = infiniteLoop:"Hello" -- return 6

data IntWrapper = IntWrapper !Int

f ('a' : 'b' : rest) = rest
f _                  = "ok"

newtype NTInt = NTInt Int deriving (Show)
data SInt = SInt !Int deriving (Show)

uNTInt = NTInt undefined
uSInt = SInt undefined

testNT = case uNTInt of NTInt _ -> True

testS = case uSInt of SInt _ -> True

data TwoInts = TwoInts {-# UNPACk #-} !Int {-# UNPACK #-} !Int
