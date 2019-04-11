--Sin listas
--todosAPartirDe :: Int -> [Int]

todosAPartirDe numeroBase tope 
    | tope >= numeroBase = [numeroBase] ++ todosAPartirDe (numeroBase+1) tope
    | otherwise = []

fibonacci 0 = 0    
fibonacci 1 = 1
fibonacci posicion = fibonacci (posicion-1) + fibonacci (posicion-2)


--Con listas

foldl' fx seed [x] = fx seed x
foldl' fx seed (x:xs) =foldl' fx (fx seed x) xs 

sum' [x] = x
sum' (x:xs) = x + sum' xs


-- fibonacciEntera 1 = [0,1]
--fibonacciEntera comienzo = [fibonacci comienzo] ++ fibonacciEntera (comienzo+1)

fibonacciEntera n nMasUno = [n+nMasUno] ++ fibonacciEntera nMasUno (n+nMasUno) 


sumoUltimosDos = sum.(take 2).reverse

{-ordenar criterio [x] listaOrdenada = 
ordenar criterio (x:xs) listaOrdenada = -}