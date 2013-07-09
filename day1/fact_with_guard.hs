module Main where

factorial :: Integer -> Integer
factorial n
  | n > 0 = n * factorial (n - 1)
  | otherwise = 1
