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