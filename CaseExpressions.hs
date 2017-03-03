module CaseExpressions where

funcZ :: Integer -> String
funcZ x = 
  case x + 1 == 1 of
  True -> "AWESOME"
  False -> "wut"

pal :: String -> String
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' :: String -> String
pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

funcC :: Ord a => a -> a -> a
funcC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n =
  case mod n 2 == 0 of
    True -> n + 2
    False -> n

nums :: (Num a, Ord a) => a -> Integer
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0