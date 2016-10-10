-- module TypeInference1 where
--   f:: Num a => a -> a -> a
--   f x y = x + y + 3

module TypeInference2 where
  f x y = x + y + 3

  -- demonstrating type inference - both compile and run the same
  -- still remember to explicitly declare type. good style.
