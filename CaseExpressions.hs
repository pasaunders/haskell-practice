module CaseExpressions where

funcZ :: Integer -> String
funcZ = case x + 1 == 1 of
  True -> "AWESOME"
  False -> "wut"

pal :: List -> String
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' :: List -> String
pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

funcC ::
funcC x y =
  case
