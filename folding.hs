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

