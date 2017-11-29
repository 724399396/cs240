module Main where

import           Test.Hspec
import           Test.QuickCheck
import           Trahs
import           Control.Lens
import qualified Data.Map.Strict          as Map

main :: IO ()
main = hspec $ describe "Testing Lab 3" $ do
  -- example quickcheck test in hspec.
  describe "read" $ do
    it "is inverse to show" $ property $
      \x -> (read . show) x == (x :: Int)

  describe "dataBase" $ do
    it "replica id is random in range" $ do
      db <- dataBase "."
      (db^.clientReplicaId) `shouldSatisfy` (\y -> y >= minBound && y <= maxBound)

    it "local version is from 1" $ do
      db <- dataBase "."
      (db^.localVersion) `shouldBe` 1

    it "version info should be empty when initial" $ do
      db <- dataBase "."
      (db^.versionInfos) `shouldBe` Map.empty

    it "file info should be empty when initial" $ do
      db <- dataBase "."
      (db^.fileInfos) `shouldBe` Map.empty
