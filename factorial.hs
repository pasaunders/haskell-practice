module Factorial where

fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n - 1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

 -- takes two arguments - starting number and number of times to increment.
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

-- recursively composes function f with applyTimes n times. End result: f . f . f . f... for n reptitions
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

