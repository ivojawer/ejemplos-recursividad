
foldl' fx seed [x] = fx seed x
foldl' fx seed (x:xs) =foldl' fx (fx seed x) xs 

sum' [x] = x
sum' (x:xs) = x + sum' xs