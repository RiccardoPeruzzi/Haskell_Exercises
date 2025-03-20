-- Esercizio 1 degli esercizi del Polimorfismo
duplicati :: Eq a => [a] -> Bool
duplicati [] = False
duplicati (x : xs) = x `elemento` xs || duplicati xs
    where 
        --elemento :: a -> [a] -> Bool
        elemento _ [] = False
        elemento y (x : _ ) | x == y = True
        elemento y (_ : xs) = elemento y xs

        -- Variante 2
        -- elemento y (x : xs) | x == y = True
        --                     | otherwise = elemento y xs

        -- Variante 3
        -- elemento y (x : xs) = x == y || y `elmento` xs
       
        -- Variante 4
        --elemento _ [] = False
        --elemento y (y : xs) = if y == x then True else elemento y xs

        

-- Algoritmo quick sort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort (ys) ++ [x] ++ qsort (zs)
        where
            ys = filter lex xs
            zs = filter gtx xs
            lex y = y <= x
            gtx y = y > x

-- si può rifare con le lamba-espressioni

qsort1 :: Ord a => [a] -> [a]
qsort1 [] = []
qsort1 (x : xs) = qsort1 (ys) ++ [x] ++ qsort1 (zs)
        where
            ys = filter (\y -> y <= x) xs
            zs = filter (\y -> y > x) xs 