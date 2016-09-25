module TakeDrop where

takeFunction :: String -> String
takeFunction x = take 16 x

takeDropFunction :: String -> String
takeDropFunction x = take 1(drop 4 x)

dropFunction :: String -> String
dropFunction x = drop 9 x

charFunction :: String -> Char
charFunction x = x !! 3

rvrs :: String -> String
rvrs x = let
  cur = take 5 x
  is = take 2 (drop 6 x)
  awe = take 7 (drop 9 x)
  in concat [awe, " ", is, " ", cur]
