module TypeKwonDo where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (==) y (f x)

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = fromInteger x
