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

  describe "mergeInfo" $ do
    let rid = 10
        lversion = 3
        localVersion = VersionInfo 1 1 False
        otherVersion = VersionInfo 2 2 False
        md5 = "abc"
    it "keep version when file not change" $ do
      let localFileInfo = Map.fromList [("file1", FileInfo md5)
                                       ,("file2", FileInfo md5)]
          vis = Map.fromList [("file1", [localVersion, otherVersion])
                             ,("file2", [localVersion])]
          db = Database rid lversion vis localFileInfo
          ndb = mergeInfo db localFileInfo
      ndb `shouldBe` db

    it "increase version when old file change" $ do
      let oldFileInfo = Map.fromList [("file1", FileInfo md5)
                                     ,("file2", FileInfo md5)]
          ovis = Map.fromList [("file1", [localVersion, otherVersion])
                              ,("file2", [localVersion])]
          nvis = Map.fromList [("file1", [localVersion+1, otherVersion])
                              ,("file2", [localVersion+1])]
          nmd5 = "sk"
          newFileInfo = Map.fromList [("file1", FileInfo nmd5)
                                     ,("file2", FileInfo nmd5)]
          db = Database rid lversion ovis oldFileInfo
          ndb = mergeInfo db newFileInfo
      ndb `shouldBe` Database rid lversion
        (Map.fromList [("file1", [VersionInfo rid lversion False, otherVersion])
                      ,("file2", [VersionInfo rid lversion False])])
        newFileInfo
