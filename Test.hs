module Main where

fractorial n = if n == 0 then 1 else n * fractorial (n - 1)

main = do putStrLn "What is 5! ?"
          x <- readLn
          if x == fractorial 5
            then putStrLn "You're right!"
            else putStrLn "You're wrong!"
