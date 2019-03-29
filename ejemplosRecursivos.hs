
foldl' fx seed [x] = fx seed x
foldl' fx seed (x:xs) =foldl fx (fx seed x) xs 