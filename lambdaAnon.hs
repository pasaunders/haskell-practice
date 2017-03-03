module LambdaAnon where

addOneIfOdd :: Integer -> Integer
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addFive x = \y -> (if x > y then y else x) + 5

mflip :: (a -> b -> c) -> b -> a -> c
mflip f x y = f y x
