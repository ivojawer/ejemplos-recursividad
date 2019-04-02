--Sin listas
--todosAPartirDe :: Int -> [Int]

todosAPartirDe numeroBase tope 
    | tope >= numeroBase = [numeroBase] ++ todosAPartirDe (numeroBase+1) tope
    | otherwise = []





--Con listas

foldl' fx seed [x] = fx seed x
foldl' fx seed (x:xs) =foldl' fx (fx seed x) xs 

sum' [x] = x
sum' (x:xs) = x + sum' xs




{-ordenar criterio [x] listaOrdenada = 
ordenar criterio (x:xs) listaOrdenada = -}