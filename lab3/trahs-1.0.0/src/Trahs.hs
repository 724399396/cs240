module Trahs (trahs, compareHistory, compareFile, CompareResult(..)) where

import           Data.List
import           Data.Set  (fromList, toList)

trahs :: IO ()
trahs = return ()

data CompareResult = Prefix | Equal | Postfix | Conflict | Delete | Create
  deriving (Show, Eq)

type FileName = String

type History = [String]
type FileInfo = (FileName, History)
type FileInfos = [FileInfo]

compareFile :: FileInfos -> FileInfos -> [(FileName,History,CompareResult)]
compareFile this that = let
  allFiles = toList $ fromList $ map fst (this ++ that)
  help Nothing (Just (f,h))        = (f,h,Create)
  help (Just (f,_)) Nothing        = (f,[],Delete)
  help (Just (f,h1)) (Just (_,h2)) = compareHistory f h1 h2
  help _ _                         = error "compare not full"
  in
  flip map allFiles $ \f -> help (find (\(f1, _) -> f == f1) this)
                       (find (\(f2, _) -> f == f2) that)

compareHistory :: FileName -> History -> History -> (FileName,History,CompareResult)
compareHistory fs hs1 hs2 = let
  zipRes = zipWith (==) hs1 hs2
  l1 = length hs1
  l2 = length hs2
  in
    if (all id zipRes)
    then if (l1 == l2)
         then (fs,hs1,Equal)
         else if (l1 < l2)
              then (fs,hs2, Prefix)
              else (fs,hs1, Postfix)
    else (fs,hs1, Conflict)

sync :: CompareResult -> IO ()
sync Equal    = return ()
sync Prefix   = syncFromOther
sync Postfix  = return ()
sync Conflict = reportConflict
sync Create   = return ()
sync Delete   = deleteThis

syncFromOther :: IO ()
syncFromOther = return ()

reportConflict :: IO ()
reportConflict = return ()

deleteThis :: IO ()
deleteThis = return ()

handleConflict :: History -> History -> IO ()
handleConflict hs1 hs2 = do
  writeHistorySelf
  readFromOther

writeHistorySelf = return ()
readFromOther = return ()
