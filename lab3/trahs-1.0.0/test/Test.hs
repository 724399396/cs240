module Main where

import Test.Hspec
import Test.QuickCheck

import Trahs

main :: IO ()
main = hspec $ describe "Testing Lab 3" $ do

  -- HUnit/HSpec  tests.
  describe "main" $ do
    it "is main" $ do
      x <- trahs
      x `shouldBe` ()

  describe "compareHistory" $ do
    it "prefix" $ do
      (compareHistory ["rectangle"] ["rectangle", "triangle"]) `shouldBe` Prefix
    it "postfix" $ do
      (compareHistory ["rectangle", "triangle"] ["rectangle"]) `shouldBe` Postfix
    it "Conflict" $ do
      (compareHistory ["rectangle", "triangle"] ["rectangle", "start"]) `shouldBe` Conflict
    it "equal" $ property $
      \x -> compareHistory x x == Equal

  describe "compareFile" $ do
    it "should get empty when input empty" $ do
      compareFile [] [] `shouldBe` []
    it "should get each situation" $ do
      compareFile [("new", [""]), ("")]

  -- example quickcheck test in hspec.
  describe "read" $ do
    it "is inverse to show" $ property $
      \x -> (read . show) x == (x :: Int)



