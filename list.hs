module Lists where

eftBool :: Bool -> Bool -> [Bool]
eftBool False True =  [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool _ _ = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT, EQ]
eftOrd LT LT = [LT]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd EQ EQ = [EQ]
eftOrd GT GT = [GT]
eftOrd _ _  = []

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x == y = [y]
  | otherwise = x : eftInt (x + 1) y

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x == y = [y]
  | otherwise = x : eftChar (succ x) y

myWords :: String -> [String]
myWords [] = []
myWords a = (takeWhile (/= ' ') a) : myWords (drop 1 (dropWhile (/= ' ') a))

myLines :: String -> [String]
myLines [] = []
myLines a = (takeWhile (/= '\n') a) : myLines (drop 1 (dropWhile (/= '\n') a))

myParameters :: String -> Char -> [String]
myParameters [] _ = []
myParameters a b = (takeWhile (/= b) a) : myParameters (drop 1 (dropWhile (/= b) a)) b