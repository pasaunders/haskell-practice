module SelfStandard where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x :xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = if f x == True then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y : ys) = if y == x then True else myElem x ys

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny _ [] = False
myElemAny x y = if any (== x) y  then True else False

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain list = squishMap (\y -> y) list

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x : y : xs)
  | (f x y == GT) || (f x y == EQ) = myMaximumBy f (x : xs)
  | f x y == LT = myMaximumBy f (y : xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x : y : xs)
  | (f x y == LT) || (f x y == EQ) = myMaximumBy f (x : xs)
  | f x y == GT = myMaximumBy f (y : xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum x = myMaximumBy compare x

myMinimum :: (Ord a) => [a] -> a
myMinimum x = myMinimumBy compare x