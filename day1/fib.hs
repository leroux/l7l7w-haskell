module Main where

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 2
fib x = fib (x - 1) + fib (x - 2)
