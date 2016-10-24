module EqExercise where

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  TisAn == TisAn = True

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  Integers Integer
