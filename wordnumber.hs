module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord 0 = "zero"

digits :: Int -> [Int]
digits n 
  | (0 <= n) && (n <= 9) = [mod n 10]
  | otherwise = (digits (div n 10)) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))