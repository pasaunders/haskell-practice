module Cipher where

import Data.Char

caesar :: String -> String
caesar message num = map (\x -> wordShift x num) (words message)

wordShift :: String -> Char
wordShift (x : xs) num = 