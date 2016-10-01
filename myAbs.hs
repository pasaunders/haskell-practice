module MyAbs where

myAbs :: Integer -> Integer
myAbs x = if x < 0
  then x * (-1)
  else x


f :: (a,b) -> (c,d) -> ((b,d),(a,c))
f x y = ((snd x,snd y),(fst x,fst y))
