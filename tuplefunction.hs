module TupleFunctions where
-- several different ways of using pattern matching with tuples

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

exercise3 :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
exercise3 (x, y, z) (w, e, f) = ((a, d), (c, f))

