size [] = 0
size (_:xs) = 1 + size xs

sizeFoldr = foldr (\ _ x -> 1 + x) 0

prod [] = 1
prod (x:xs) = x * prod xs

prodFoldr = foldr1 (*)
