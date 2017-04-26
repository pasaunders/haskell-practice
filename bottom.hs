module Bottom where

f :: Bool -> Int
f True = error "blah"
f False = 0

data Maybe a = Nothing | Just a

f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)




dividedBy :: Integral a => a-> a-> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)