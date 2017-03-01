module CaseExpressions where

funcZ :: Integer -> String
funcZ = 
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

funcC :: a-> b -> Either a b
funcC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 :: Num a => a -> a
ifEvenAdd2 n =
  case mod n 2 == 0 of
    True -> n + 2
    False -> n