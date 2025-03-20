-- scrive i numeri da 1 a n 
crea :: Int -> [Int]
crea 0 = [] 
crea n = crea (n - 1) ++ [n] 

-- seconda soluzione
crea1 n = take n (da 1)  -- prende k numeri in fila
    where 
        da k = k : da (k + 1) -- genera la lista da 1 a infinto

-- funzione per la lunghezza di una lista
lunghezza :: [Int] -> Int
lunghezza [] = 0    --per la lista nil
lunghezza (x : xs) = 1 + lunghezza xs

-- funzione per la somma degli elementi di una lista
somma :: [Int] -> Int
somma [] = 0
somma (x : xs) = x + somma xs

-- prodotto di elementi in una lista di coppie

f :: [(Int,Int)] -> [Int]
f [] = []
f ((x,y) : xs) = x * y : f xs

-- presa una lista, produce la lista di numeri interi pari delle lista di partenza

g :: [Int] -> [Int]
g [] = []
g (x : xs) | even x = x : g xs
           | otherwise = g xs

-- definire una funzione che applicata a due liste produca la lista formata da coppie con gli elementi corrispondenti
-- questa versione è parziale perchè non dice come si comporta se viene applicata a due liste con lunghezza differente

chiudi :: [Int] -> [Int] -> [(Int, Int)]
chiudi [] [] = []
chiudi (x : xs) (y : ys) = (x, y) : chiudi xs ys

-- versione totale
chiudi1 :: [Int] -> [Int] -> [(Int, Int)]
chiudi1 [] _ = []
chiudi1 _ [] = []
chiudi1 (x : xs) (y : ys) = (x, y) : chiudi1 xs ys


apri :: [(Int,Bool)] -> ([Int],[Bool])
apri [] = ([],[])
apri ((x,y) : ps) = (x : xs, y : ys)  -- prima di ps scrivo il pattern che mi aspetto di vedere in quella posizione cioè p --> (x,y)
    where
        (xs,ys) = apri ps