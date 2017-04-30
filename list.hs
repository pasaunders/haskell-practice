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