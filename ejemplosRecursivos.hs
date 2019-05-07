--Sin listas
--todosAPartirDe :: Int -> [Int]

todosAPartirDe numeroBase tope 
    | tope >= numeroBase = numeroBase : todosAPartirDe (numeroBase+1) tope
    | otherwise = []

fibonacci 0 = 0    
fibonacci 1 = 1
fibonacci posicion = fibonacci (posicion-1) + fibonacci (posicion-2)


--Con listas

foldl' fx seed [x] = fx seed x
foldl' fx seed (x:xs) =foldl' fx (fx seed x) xs 

sum' [x] = x
sum' (x:xs) = x + sum' xs

fibonacciEntera = fibonacciAPartirDePos 0

fibonacciAPartirDePos pos = fibonacciAPartirDe (fibonacci pos) (fibonacci (pos+1))

fibonacciAPartirDe n proxN = n : fibonacciAPartirDe proxN (n+proxN) 


sumoDosATodos [] = []
sumoDosATodos (x:xs) = (x+2) : sumoDosATodos xs


leHagoATodos _ [] = []
leHagoATodos algo (x:xs) = algo x : leHagoATodos algo xs