module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Trahs

main :: IO ()
main = hspec $ describe "Testing Lab 3" $ do

  -- HUnit/HSpec  tests.
  describe "main" $ do
    it "is main" $ do
      x <- trahs
      x `shouldBe` ()

  describe "compareHistory" $ do
    it "prefix" $ do
      (compareHistory "" ["rectangle"] ["rectangle", "triangle"]) `shouldBe` ("",["rectangle", "triangle"], Prefix)
    it "postfix" $ do
      (compareHistory "" ["rectangle", "triangle"] ["rectangle"]) `shouldBe` ("", ["rectangle", "triangle"], Postfix)
    it "Conflict" $ do
      (compareHistory "" ["rectangle", "triangle"] ["rectangle", "start"]) `shouldBe` ("", ["rectangle", "triangle"], Conflict)
    it "equal" $ property $
      \x -> compareHistory "" x x == ("", x, Equal)

  describe "compareFile" $ do
    it "should get empty when input empty" $ do
      compareFile [] [] `shouldBe` []
    it "should get each situation" $ do
      compareFile [("new", [""]), ("prefix", ["hs1"]), ("postfix", ["hs1", "hs2"]), ("conflict", ["hs1","hs2"])] [("delete",[""]), ("prefix",["hs1", "hs2"]), ("postfix",["hs1"]), ("conflict", ["hs1", "hs3"])] `shouldBe` [("new", [""], Create), ("delete", [""], Delete), ("prefix", ["hs1", "hs2"], Prefix), ("postfix", ["hs1","hs2"], Postfix), ("conflict", ["hs1", "hs3"], Conflict)]

  -- example quickcheck test in hspec.
  describe "read" $ do
    it "is inverse to show" $ property $
      \x -> (read . show) x == (x :: Int)



