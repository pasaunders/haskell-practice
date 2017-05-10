module Folding where

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z xs =
  case xs of
    []     -> z
    (x:xs) -> f x (myFoldr f z xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

foldl (flip (*)) 1 [1..5] == (5 * (4 * (3 * (2 * (1 * 1)))))