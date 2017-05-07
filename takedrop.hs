module TakeDrop where

import Data.Char

takeFunction :: String -> String
takeFunction x = take 16 x

takeDropFunction :: String -> String
takeDropFunction x = take 1(drop 4 x)

dropFunction :: String -> String
dropFunction x = drop 9 x

charFunction :: String -> Char
charFunction x = x !! 3

rvrs :: String -> String
rvrs x = let
  cur = take 5 x
  is = take 2 (drop 6 x)
  awe = take 7 (drop 9 x)
  in concat [awe, " ", is, " ", cur]

mySqr :: Int -> [Int]
mySqr x = [x^2 | x <- [1..5]]

myCube :: Int -> [Int]
myCube x = [x^3 | x <- [1..5]]

-- [x | x <- mySqr, rem x 2 == 0]
-- [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

-- [(x, y) | x <- mySqr, y <- myCube]
-- [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

-- map (\x -> if x == 3 then (-x) else (X)) [1..10]

mysteryFunction :: String -> [Bool]
mysteryFunction xs = map (\x -> elem x "aeiou") xs


myFilter :: [Int] -> [Int]
myFilter x = filter (\y -> (mod y 3) == 0) x   -- ask ben about non-lambda alternatives

numFiltered :: [Int] -> Int
numFiltered = length . myFilter

sentenceFilter :: String -> [String]
sentenceFilter x = filter  (\y -> not (y == "a" || y == "an" || y == "the")) (words x)

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

upperFilter :: String -> String
upperFilter x = filter (\y -> not (isUpper y)) x

lowerFilter :: String -> String
lowerFilter x = filter isUpper x

capString :: String -> String
capString [] = []
capString (x : xs) = [toUpper x] ++ xs

recurseAllcaps :: String -> String
recurseAllcaps [] = []
recurseAllcaps (x : xs) = [toUpper x] ++ recurseAllcaps xs

firstCap :: String -> Char
firstCap x = toUpper . head $ x

firstCapPointFree :: String -> Char
firstCapPointFree = toUpper . head