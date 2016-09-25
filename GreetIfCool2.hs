module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool == "downright frosty yo"
    then putStrLn "eyyyyy. What's shakin'?"
  else
    putStrLn "pshhh."
