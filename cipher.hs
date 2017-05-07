module Cipher where

import Data.Char

caesar :: String -> Int -> String
caesar [] _ = []
caesar (x : xs) num
  | x == ' ' = ' ' : caesar xs num
  | ord x <= 90 = chr ((mod (ord x + num - 65) 26) + 65)  : caesar xs num
  | ord x <= 122 = chr ((mod (ord x + num - 97) 26) + 97) : caesar xs num

unCaesar :: String -> Int -> String
unCaesar [] _ = []
unCaesar (x : xs) num
  | x == ' ' = ' ' : unCaesar xs num
  | ord x <= 90 = chr ((mod (ord x - num - 65) 26) + 65)  : unCaesar xs num
  | ord x <= 122 = chr ((mod (ord x - num - 97) 26) + 97) : unCaesar xs num