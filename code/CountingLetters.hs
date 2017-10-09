import Data.Char

countLowerCase :: String -> Int
countLowerCase = length (filter isLower str)

countLowercaseAndDigits :: String -> Int
countLowercaseAndDigits = length . filter (\c -> isLower c || isDigit c)

stripPunctuation :: String -> String
stripPunctuation = filter (`notElem` "!#$%&*+./<=>?@\\^|-~:")
