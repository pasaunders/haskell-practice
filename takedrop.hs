module TakeDrop where

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