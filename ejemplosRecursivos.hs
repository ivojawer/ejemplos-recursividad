--Sin listas

todosAPartirDe numeroBase tope 
    | tope >= numeroBase = numeroBase : todosAPartirDe (numeroBase+1) tope
    | otherwise = []

fibonacci 0 = 0    
fibonacci 1 = 1
fibonacci posicion = fibonacci (posicion-1) + fibonacci (posicion-2)



factorial 0 = 1
factorial n = factorial (n-1) * n

--Con listas
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' [x] = x
sum' (x:xs) = x + sum' xs

any' _ [] = False
any' f (x:xs) = f x || any' f xs 

anyQueCortaRecursividad _ [] = False
anyQueCortaRecursividad f (x:xs) 
    | f x = True 
    | otherwise = anyQueCortaRecursividad f xs

map' _ [] = []
map' algo (x:xs) = algo x : map' algo xs

--Extras

foldl' fx seed [x] = fx seed x
foldl' fx seed (x:xs) =foldl' fx (fx seed x) xs 

fibonacciDeInfinita posicion =  fibonacciEntera !! posicion

fibonacciEntera = fibonacciAPartirDePos 0

fibonacciAPartirDePos pos = fibonacciAPartirDe (fibonacci pos) (fibonacci (pos+1))

fibonacciAPartirDe n proxN = n : fibonacciAPartirDe proxN (n+proxN) 

