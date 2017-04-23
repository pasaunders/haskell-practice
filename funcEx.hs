module FuncEx where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = (fst . divMod x) 10
        d     = (snd . divMod xLast) 10

hundDigit :: Integral a => a -> a
hundDigit x = d
  where xLast = (fst . divMod x) 100
        d     = (snd . divMod xLast) 10

foldBool :: a -> a -> Bool -> a
foldBool x y z
  | z == True = x
  | z == False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x , y) = (f x, y)

