module Day2 where

import Data.List.Split

mySort :: Ord a => [a] -> [a]
mySort [] = []
mySort (x:xs) = lesser xs ++ [x] ++ greater xs
  where lesser = mySort . filter (<= x)
        greater = mySort . filter (> x)

mySortBy :: (a -> a -> Ordering) -> [a] -> [a]
mySortBy _ [] = []
mySortBy f xs = undefined

moneyToNum ::  String -> Double
moneyToNum xs = read [x | x <- xs, x `elem` ['0'..'9'] || x == '.'] :: Double

lazyThirds ::  (Enum t, Num t) => t -> [t]
lazyThirds a = [a, a + 3 ..]

lazyFifths ::  (Enum t, Num t) => t -> [t]
lazyFifths a = [a, a + 5 ..]

lazyEights ::  (Enum c, Num c) => c -> c -> [c]
lazyEights a b = zipWith (+) (lazyThirds a) (lazyFifths b)

half ::  Double -> Double
half = (* 0.5)

appendNewline :: String -> String
appendNewline = (++ "\n")

myGcd ::  Integral a => a -> a -> a
myGcd a b = myGcd' (abs a) (abs b)
            where myGcd' a 0 = a
                  myGcd' a b = myGcd' b $ a `mod` b

isPrime ::  Integral a => a -> Bool
isPrime a = 0 `notElem` zipWith mod (cycle [a]) [2 .. (a - 1)]

lazyPrimes ::  [Integer]
lazyPrimes = filter isPrime [2..]

myLines ::  (Enum a, Num a) => [Char] -> [(a, [Char])]
myLines xs = zip [1..] $ splitOn "\n" xs

myWords :: (Enum a, Num a) => [Char] -> [(a, [Char])]
myWords xs = zip [1..] $ splitOn " " xs
