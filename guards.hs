module Guards where

avgGrade1 :: (Fractional a, Ord a) => a -> Char
avgGrade1 x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y <= 0.59 = 'F'
  where y = x / 100

avgGrade2 :: (Fractional a, Ord a) => a -> Char
avgGrade2 x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | y <= 0.59 = 'F'
  where y = x / 100