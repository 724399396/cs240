module Main (main) where

import Test.Hspec

import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do

    describe "empty pattern" $ do
      it "matches empty string" $
        matchGlob "" "" `shouldBe` True
      it "shouldn't match non-empty string" $
        matchGlob "" "string" `shouldBe` False
    describe "literal matching" $ do
      it "should match exactly character" $
        matchGlob "abcde" "abcde" `shouldBe` True
      it "shouldn t match not-match character" $
        matchGlob "b" "ab" `shouldBe` False
      it "should think ] is a normal character" $
        matchGlob "a]b" "a]b" `shouldBe` True
      it "match \\character as character" $
        matchGlob "\a\b\\c\\d\\e" "abcde" `shouldBe` True
      it "match - as a normal character" $
        matchGlob "-adf]ai1" "-adf]ai1" `shouldBe` True
    describe "escaped literals" $ do
      it "should treade \\character as character" $
        matchGlob "\\[a]" "[a]" `shouldBe` True
      it "should escape \\ self" $
        matchGlob "\\*\\*\\?" "**?" `shouldBe` True
      it "should escape \\" $
        matchGlob "\\a\\" "\a\"" `shouldBe` True
      it "should escape *" $
        matchGlob "ab\\*ba" "ab*ba" `shouldBe` True
      it "should escape [" $
        matchGlob "ab\\[ba" "ab[ba" `shouldBe` True
    describe "set match" $ do
      it "match normal characters" $
        matchGlob "[abcd]" "c" `shouldBe` True
      it "match range" $
        matchGlob "[a-z]" "w" `shouldBe` True


