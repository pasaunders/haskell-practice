module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = (read . show) a

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = (read . show)

roundTripType :: (Show a, Read b) => a -> b
roundTripType = (read . show)

main = do
  print (roundTrip 4)
  print (roundTripPF 5)
  -- print (roundTripType 6)