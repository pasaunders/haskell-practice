module Bottom where

f :: Bool -> Int
f True = error "blah"
f False = 0

g :: Bool -> Maybe Int
g False = Just 0
g _ = Nothing

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

data DividedResult =
  Result Integer | DividedByZero


-- ask ben how to do all this, see textbook pg 293
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | d == 0 = DividedByZero
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

lineAddition :: (Eq a, Num a) => a -> a
lineAddition 0 = 0
lineAddition n = n + lineAddition(n - 1)

multiplyBy :: Integral a => a -> a ->a
multiplyBy factorOne factorTwo = go 0 factorTwo 0
  where go factor target count
          | count == target = factor
          | otherwise = go (factor + factorOne) target (count + 1)