module Globber (matchGlob) where

type GlobPattern = String

data Match = Literal Char | Escape Char | Set [Char] |
             Single | Any

indexOf :: (Ord a) -> a -> [a] -> Maybe Int
indexOf x xs = case filter ((==x) . fst) (zip xs [0..]) of
                 [] -> Nothing
                 r -> Just $ snd $ head r

setElem :: String -> ([Char], String)
setElem str = case indexOf '-' of
                Just x ->

parsePattern :: String -> [Match]
parsePattern (x:xs) =
  case x of
  '\\' -> (Escape (head xs)) : (parsePattern $ tail xs)
  '?' -> Single
  '*' -> Any
  '[' ->
  _ -> (Literal x) : (parsePattern xs)
parsePattern [] = []

matchGlob :: GlobPattern -> String -> Bool
matchGlob _ _ = False
