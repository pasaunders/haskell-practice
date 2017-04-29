module Lists where

eftBool :: Bool -> Bool -> [Bool]
eftBool False True =  [False, True]
eftBool False False = [False]
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd =