allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (x:xs) = if even x then x : allEven xs else allEven xs

allEven' xs = [x | x <- xs, even x]

allEven'' = filter even

allEven''' = filter (\x -> x `mod` 2 == 0)
