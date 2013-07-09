main :: IO ()
main = undefined

allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (x:xs) = if even x then x : allEven xs else allEven xs
allEven' xs = [x | x <- xs, even x]
allEven'' = filter even
allEven''' = filter (\x -> x `mod` 2 == 0)

rev [] = []
rev (x:xs) = rev xs ++ [x]
rev' = foldr (\x acc -> acc ++ [x]) []

colorCombos = let colors = ["black", "white", "blue", "yellow", "red"]
              in  [(a, b) | a <- colors, b <- colors, a < b]

childMultTable = [(x, y, x * y) | x <- [1..12], y <- [1..12]]
childMultTable' = [(x, y, x * y) | x <- [1..12], y <- [1..12], x <= y]

mapColoring = let colors = ["red", "green", "blue"]
                  states = ["MI", "TN", "AL", "GA", "FL"]
                  statesAdj = [("MI", "TN"), ("MI", "AL"), ("AL", "TN"), ("AL", "MI"), ("AL", "GA"),
                               ("AL", "FL"), ("GA", "FL"), ("GA", "TN")]
              in [(state, color) | state <- states, color <- colors]
