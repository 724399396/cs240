{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Word (Word16)
import Data.Char (ord)
import Data.Bits ((.&.), shiftR)
import Test.HUnit (assertEqual)
import Test.QuickCheck hiding ((.&.))
import Control.Arrow (first)
import System.Random

encodeChar :: Char -> [Word16]
encodeChar x
  | w < 0x10000 = [fromIntegral w]
  | otherwise   = [fromIntegral a, fromIntegral b]
  where w = ord x
        a = ((w - 0x10000) `shiftR` 10) + 0xD800
        b = (w .&. 0x3FF) + 0xDC00

testOne char =
  assertEqual "ASCII encodes as one code unit"
    1 (length (encodeChar char))

testASCII = mapM_ testOne ['\0'..'\127']

badTest = do
  assertEqual "sestertius encodes as one code unit"
    1 (length (encodeChar '\x10198'))

prop_encodeOne c = length (encodeChar c) == 1

newtype BigChar = Big Char
  deriving (Eq, Show, Random, Ord)

instance Arbitrary BigChar where
  arbitrary = choose (Big '0', Big '\x10FFFF')
  shrink (Big c) = map Big (shrinkChar c)

shrinkChar c = undefined

prop_encodeOne2 = do
  c <- choose ('\0', '\xFFFF')
  return $ length (encodeChar c) == 1

prop_encodeOne3 (Big c) = length (encodeChar c) == 1

prop_encodeOne4 (Big c) =
  (c < '\x10000') ==> length (encodeChar c) == 1

prop_encodeOne5 = do
  Big c <- arbitrary `suchThat` (< Big '\x10000')
  return $ length (encodeChar c) == 1
