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
mSqr [x^2 | x <- [1..15]]

myCube :: Int 

[x | x <- mySqr, rem x 2 == 0]
[(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]