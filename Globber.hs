module Globber (matchGlob) where

type GlobPattern = String

newtype Match = Match { match:: String -> Bool }

literal :: String -> Match
literal s = let
  escape (x:xs) = case x of
    '\\' -> (head xs) : (escape (tail xs))
    _ -> x : (escape xs)
  escape [] = []
  in Match $ \i -> i == (escape s)

matchGlob :: GlobPattern -> String -> Bool
matchGlob _ _ = False

