module Main where

import           Control.Lens
import           Data.List
import qualified Data.Map.Strict as Map
import           Test.Hspec
import           Test.QuickCheck
import           Trahs

main :: IO ()
main = hspec $ describe "Testing Lab 3" $ do
  -- example quickcheck test in hspec.
  describe "read" $ do
    it "is inverse to show" $ property $
      \x -> (read . show) x == (x :: Int)

  describe "localDatabase" $ do
    it "replica id is random in range" $ do
      db <- localDatabase "."
      (db^.replicaId) `shouldSatisfy` (\y -> y >= minBound && y <= maxBound)

    it "local version is from 1" $ do
      db <- localDatabase "."
      (db^.versionId) `shouldBe` Version 1

    it "version info should be empty when initial" $ do
      db <- localDatabase "."
      (db^.dbVersionInfo) `shouldBe` Map.empty

    it "file info should be empty when initial" $ do
      db <- localDatabase "."
      (db^.dbFileInfo) `shouldBe` Map.empty

  describe "mergeLocalInfo" $ do
    let lrid = ReplicaId 10
        orid = ReplicaId 11
        file1 = "file1"
        file2 = "file2"
        versionForLocalFile1 = Version 3
        versionForOtherFile1 = Version 10
        versionForLocalFile2 = Version 4
        currentVersion = Version 5
        localVersionInfo = Map.fromList [((file1, lrid), versionForLocalFile1)
                                        ,((file1, orid), versionForOtherFile1)
                                        ,((file2, lrid), versionForLocalFile2)]
        file1Hash = "hash1"
        file2Hash = "hash2"
        localOriginFileInfo = Map.fromList [(file1, file1Hash)
                                     ,(file2, file2Hash)]
        localDb = Database lrid currentVersion localVersionInfo localOriginFileInfo
    it "keep version when file not change" $ do
      let ndb = mergeLocalInfo localDb localOriginFileInfo
      ndb `shouldBe` localDb

    it "increase current replica version and keep other replica version when old file change" $ do
      let newHash = "newHash"
          newFileInfo = Map.fromList [(file1, newHash)
                                     ,(file2, newHash)]
          ndb = mergeLocalInfo localDb newFileInfo
      ndb `shouldBe` Database lrid currentVersion (Map.fromList [((file1, lrid), currentVersion)
                                        ,((file1, orid), versionForOtherFile1)
                                        ,((file2, lrid), currentVersion)]) newFileInfo

    it "increase version and delete file info when old file deleted" $ do
      let newFileInfo = Map.fromList []
          ndb = mergeLocalInfo localDb newFileInfo
      ndb `shouldBe` Database lrid currentVersion (Map.fromList [((file1, lrid), currentVersion)
                                        ,((file1, orid), versionForOtherFile1)
                                        ,((file2, lrid), currentVersion)]) newFileInfo

    it "add new file version when add file" $ do
      let file3 = "file3"
          file3Hash = "file3Hash"
          newFileInfo = Map.fromList [(file1, file1Hash)
                                     ,(file2, file2Hash)
                                     ,(file3, file3Hash)]
          ndb = mergeLocalInfo localDb newFileInfo
      ndb `shouldBe` Database lrid currentVersion (Map.fromList [((file1, lrid), versionForLocalFile1)
                                        ,((file1, orid), versionForOtherFile1)
                                        ,((file2, lrid), versionForLocalFile2)
                                        ,((file3, lrid), currentVersion)]) newFileInfo

  describe "compareDb" $ do
    let lrid = ReplicaId 10
        orid = ReplicaId 11
        file1 = "file1"
        file2 = "file2"
        versionForLocalFile1 = Version 3
        versionForOtherFile1 = Version 1
        versionForLocalFile2 = Version 4
        currentVersion = Version 5
        otherVersion = Version 6
        localVersionInfo = Map.fromList [((file1, lrid), versionForLocalFile1)
                                        ,((file1, orid), versionForOtherFile1)
                                        ,((file2, lrid), versionForLocalFile2)]
        file1Hash = "hash1"
        file2Hash = "hash2"
        localOriginFileInfo = Map.fromList [(file1, file1Hash)
                                     ,(file2, file2Hash)]
    it "keep on same key when file not change" $ do
      let localDb = Database lrid currentVersion localVersionInfo localOriginFileInfo
          otherDb = Database orid otherVersion localVersionInfo localOriginFileInfo
          result = compareDb localDb otherDb
      (sort (result Map.! Same)) `shouldBe` (sort [file1, file2])

    it "keep on update when old file change on client" $ do
      let newHash = "newHash"
          newFileInfo = Map.fromList [(file1, newHash)
                                     ,(file2, newHash)]
          otherDb = Database orid otherVersion localVersionInfo localOriginFileInfo
          localDb = Database lrid currentVersion (Map.fromList [((file1, lrid), currentVersion)
                                        ,((file1, orid), versionForLocalFile2)
                                        ,((file2, lrid), currentVersion)]) newFileInfo
          result = compareDb localDb otherDb
      result `shouldBe` Map.empty
--      (sort (result Map.! Update)) `shouldBe` (sort [file1, file2])

    -- it "keep on update when old file change on server" $ do
    --   let newHash = "newHash"
    --       newFileInfo = Map.fromList [(file1, newHash)
    --                                  ,(file2, newHash)]
    --       otherDb = Database orid otherVersion (Map.fromList [((file1, orid), versionForLocalFile1)
    --                                     ,((file1, lrid), versionForOtherFile1)
    --                                     ,((file2, lrid), versionForLocalFile2)]) localOriginFileInfo
    --       updateLocalDb = Database lrid currentVersion (Map.fromList [((file1, lrid), currentVersion)
    --                                     ,((file1, orid), versionForOtherFile1)
    --                                     ,((file2, lrid), currentVersion)
    --                                     ,((file2, orid), versionForOtherFile2)]) newFileInfo
    --       result = compareDb updateLocalDb otherDb
    --   (sort (result Map.! Update)) `shouldBe` (sort [file1, file2])



    -- it "increase version and delete file info when old file deleted" $ do
    --   let newFileInfo = Map.fromList []
    --       ndb = mergeLocalInfo localDb newFileInfo
    --   ndb `shouldBe` Database lrid currentVersion (Map.fromList [((file1, lrid), currentVersion)
    --                                     ,((file1, orid), versionForOtherFile1)
    --                                     ,((file2, lrid), currentVersion)]) newFileInfo

    -- it "add new file version when add file" $ do
    --   let file3 = "file3"
    --       file3Hash = "file3Hash"
    --       newFileInfo = Map.fromList [(file1, file1Hash)
    --                                  ,(file2, file2Hash)
    --                                  ,(file3, file3Hash)]
    --       ndb = mergeLocalInfo localDb newFileInfo
    --   ndb `shouldBe` Database lrid currentVersion (Map.fromList [((file1, lrid), versionForLocalFile1)
    --                                     ,((file1, orid), versionForOtherFile1)
    --                                     ,((file2, lrid), versionForLocalFile2)
    --                                     ,((file3, lrid), currentVersion)]) newFileInfo
