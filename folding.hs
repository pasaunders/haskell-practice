module Folding where

import Data.Time

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z xs =
  case xs of
    []     -> z
    (x:xs) -> f x (myFoldr f z xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- foldl (flip (*)) 1 [1..5] == (5 * (4 * (3 * (2 * (1 * 1)))))

threeLetters :: [String] -> String
threeLetters xs = foldr (\ a b -> take 3 a ++ b) "" xs

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate x = foldr findDate [] x
  where findDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
        findDate (DbDate time) ys = time : ys
        findDate _ zs = zs


findDate2 :: DatabaseItem -> [UTCTime] -> [UTCTime]
findDate2 x ys =
  case x of
    DbDate z -> z : ys
    _ -> ys

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber x = foldr findNumber [] x
  where findNumber :: DatabaseItem -> [Integer] -> [Integer]
        findNumber (DbNumber number) ys = number : ys
        findNumber _ ys = ys


-- working with datatypes
-- (DbDate undefined) :: DatabaseItem
-- True :: Bool

filterDbRecent :: [DatabaseItem] -> [UTCTime]
filterDbRecent x = foldr filterRecent [] x
  where filterRecent :: DatabaseItem -> [UTCTime] -> [UTCTime]
        filterRecent (DbDate time) zs
          | zs == [] = [time]
          | time > head zs = [time]
          | otherwise = zs
        filterRecent _ zs = zs

addDatabase :: [DatabaseItem] -> Integer
addDatabase x = sum (foldr findNumber [] x)
  where findNumber :: DatabaseItem -> [Integer] -> [Integer]
        findNumber (DbNumber y) zs = y : zs
        findNumber _ zs = zs


makeWords :: String -> String -> String -> [(Char, Char, Char)]
makeWords _ _ [] = []
makeWords xs ys (z:zs) = selectFirst xs ys z ++ makeWords xs ys zs

selectFirst :: String -> String -> Char -> [(Char, Char, Char)]
selectFirst _ [] _ = []
selectFirst xs (y:ys) z = selectSecond xs y z ++ selectFirst xs ys z

selectSecond :: String -> Char -> Char -> [(Char, Char, Char)]
selectSecond [] _ _ = []
selectSecond (x:xs) y z = selectThird x y z : selectSecond xs y z

selectThird :: Char -> Char -> Char -> (Char, Char, Char)
selectThird x y z = (x, y, z)


makeWordsP :: String -> String -> [(Char, Char, Char)]
makeWordsP _ [] = []
makeWordsP xs (y:ys) = selectSecondP xs y ++ makeWordsP xs ys

selectSecondP :: String -> Char -> [(Char, Char, Char)]
selectSecondP [] _ = []
selectSecondP (x:xs) y = selectThirdP x y : selectSecondP xs y

selectThirdP :: Char -> Char -> (Char, Char, Char)
selectThirdP y z = ('p', y, z)

myOrDirect :: [Bool] -> Bool
myOrDirect [] = False
myOrDirect (x:xs)
  | x == True = True
  | x == False = myOrDirect xs

myOrDirectSymbol :: [Bool] -> Bool
myOrDirectSymbol [] = False
myOrDirectSymbol (x:xs) = x || myOrDirectSymbol xs

myOrFolding :: [Bool] -> Bool
myOrFolding = foldr (\a b -> if a == True then True else b) False

myOrFoldingPointFree :: [Bool] -> Bool
myOrFoldingPointFree = foldr (||) False

myAnyDirect :: (a -> Bool) -> [a] -> Bool
myAnyDirect _ [] = False
myAnyDirect f (x:xs)
  | f x == True = True
  | f x == False = myAnyDirect f xs

myAnyDirectSymbol :: (a -> Bool) -> [a] -> Bool
myAnyDirectSymbol _ [] = False
myAnyDirectSymbol f (x:xs) = f x || myAnyDirectSymbol f xs

myAnyFolding :: (a -> Bool) -> [a] -> Bool
myAnyFolding f xs = foldr (\a b -> if f a == True then True else b) False xs

myAnyFoldingPointFree :: (a -> Bool) -> [a] -> Bool
-- point free and typechecks, but doesn't actually work.
myAnyFoldingPointFree  = flip(foldr . ((.) (||))) False

-- Unifing types to see why the above function works.
-- (.) :: (e -> f) -> (d -> e) -> d -> f
-- (||) :: Bool -> Bool -> Bool
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldr :: [a] => (a -> b -> b) -> (b -> [a] -> b)
-- (.) foldr :: (d -> a -> b -> b) -> d -> b -> [a] -> b
-- (.) (||) :: (f -> Bool) -> f -> Bool -> Bool
--             ((f -> Bool) -> f -> Bool -> Bool) -> (f -> Bool) -> Bool -> [f] -> Bool
--                                                   (f -> Bool) -> Bool -> [f] -> Bool
-- foldr . ((.) (||)) :: (f -> Bool) -> (Bool -> ([f] -> Bool))
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip (foldr . ((.) (||))) :: Bool -> (f -> Bool) -> ([f] -> Bool)

myElemFoldr :: Eq a => a -> [a] -> Bool
myElemFoldr x xs = foldr (\a b -> (a == x) || b) False xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x xs = any (\a -> a == x) xs

myReverse :: [a] -> [a]
-- myReverse xs = foldl (\b a -> a : b) [] xs
myReverse xs = foldl (flip (:)) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\a b -> f a : b) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\a b -> if f a then a : b else b) [] xs

mySquish :: [[a]] -> [a]
mySquish xs = foldr (\a b -> a ++ b) [] xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldr (\a b -> f a ++ b) [] xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\a -> a) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if f x y == GT then x else y) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> if f x y == LT then x else y) (head xs) xs