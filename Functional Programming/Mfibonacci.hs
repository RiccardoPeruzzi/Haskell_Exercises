-- 'Prelude' è il nome della libreria standard di Haskell, di solito la importa da solo
-- ma in questo caso la uso per escludere una funzione importata
import Prelude hiding ((^))

-- Una matrice quadrata 2x2 è una coppia coppia di vettori riga,
-- si può quindi rappresentare un vettore e vedere la matrice come coppia
type Vet = (Integer, Integer)   -- vettore
type Mat = (Vet, Vet)   -- matrice (coppia di vettori)

-- Moltiplicare due matrice è fare il prodotto righe per colonne
-- fare questo è come fare il prodotto scalare tra due vettori

-- prodotto scalare tra due vettori
(•) :: Vet -> Vet -> Integer
(•) (a,b) (c,d) = a * c + b * d

-- calcolo trasposta della matrice
trasp :: Mat -> Mat 
trasp ((a,b),(c,d)) = ((a,c),(b,d)) 

-- prodotto matriciale
(×) :: Mat -> Mat -> Mat
(×) (r1, r2) m = ((r1 • c1, r1 • c2), (r2 • c1, r2 • c2)) -- prodotto righe per colonne
    where 
        (c1,c2) = trasp m -- calcola la trasposta di m e ottengo le colonne che mi servono nel prodotto matriciale

-- calcolo la potenza di una matrice (partendo dalla potenza di un elemento) "ricorsivo"
(^) :: Mat -> Int -> Mat    
(^) _ 0 = ((1, 0), (0, 1)) -- ottengo la matrice identità
(^) a k | even k = b × b
    where
        b = a ^ (k `div` 2) -- se l'esponente è pari lo dimezzo
(^) a k = a × (a ^ (k - 1))

-- calcolo sequenza fibonacci
fibo :: Int -> Integer
fibo k = snd (fst m)        -- con le proiezioni accedo alla seconda componente della prima riga
    where 
        m = a ^ k
        a = ((1,1),(1,0))