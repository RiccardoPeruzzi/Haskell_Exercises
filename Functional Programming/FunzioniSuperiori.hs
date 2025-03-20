-- funzione che nega gli elementi di una lista
negazioni :: [Bool] -> [Bool]
negazioni [] = []
negazioni (x : xs) = not x : negazioni xs

-- funzione che incrementa tutti gli elementi di una lista intera
incrementi :: [Int] -> [Int]
incrementi [] = []
incrementi (x : xs) = succ x : incrementi xs

-- OSSERVAZIONE : queste due funzioni hanno lo stesso caso base (lista vuota ridà lista vuota) e hanno il caso ricorsivo simile in quanto
--                entrambe applicano una trasformazione su tutta la lista

mappa :: (a -> b) -> [a] -> [b]
mappa f [] = []
mappa f (x : xs) = f x : mappa f xs

-- chiamo la funzione mappa applicando la negazione
negazione :: [Bool] -> [Bool]
negazione xs = mappa not xs

-- chiamo l'incremento applicando il successore
incremento :: [Int] -> [Int]
incremento xs = mappa succ xs


prodotti :: [Int] -> Int
prodotti [] = 1
prodotti (x : xs) = (*) x (prodotti xs)         -- oppure (*) x (prodotti xs)

-- Congiunzioni e prodotti permettono di combinare tutti gli elementi di una lista

riduci :: (a -> b -> b) -> b -> [a] -> b
riduci f y [] = y
riduci f y (x : xs) = f x (riduci f y xs)

congiunzioni1 :: [Bool] -> Bool
congiunzioni1 xs = riduci (&&) True xs

prodotti1 :: [Int] -> Int
prodotti1 xs = riduci (*) 1 xs

somma :: [Num] -> Num
somma xs = riduci (sum) 0 xs

-- Dice se tutti gli elementi di una lista sono pari senza la ricorsione
-- in questo modo tramite foldr e map ottengo il risultato
tuttipari :: [Int] -> Bool
tuttipari xs = foldr (&&) True (map even xs)


pari :: [Int] -> [Int]
pari [] = []
pari (x : xs) | even x = x : pari xs
              | otherwise = pari xs 

nulle :: [[a]] -> [[a]]
nulle [] = []
nulle (xs : xss) | null xs = xs : nulle xss
                 | otherwise = nulle xss

--Si può creare anche una funzione filtro in quanto le funzioni pari e nulle hanno molto in comune

filtro :: (a -> Bool) -> [a] -> [a]
filtro _ [] = []
filtro f (x : xs) | f x = x : filtro f xs
filtro f (_ : xs) = filtro f xs

pari1 :: [Int] -> [Int]
pari1 xs = filtro even xs

nulle1 :: [[a]] -> [[a]]
nulle1 xss = filtro null xss