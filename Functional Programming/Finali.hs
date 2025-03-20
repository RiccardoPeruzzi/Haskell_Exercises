somma :: Num a => [a] -> a
somma = foldr (+) 0 

media :: [Int] -> Int
media xs = aux (somma (filter even xs)) xs
    where
        aux x xs = x `div` length xs 

prodotto :: (Eq a, Num a) => [a] -> a
prodotto xs = foldr (*) 1 (filter (\x -> x /= 0) xs)

modulo :: Floating a => [a] -> a
modulo = sqrt . sum . map (^2)

lunghezza :: [a] -> Int
lunghezza = sum . map (const 1)

ordinata :: Ord a => [a] -> Bool
ordinata xs = all (uncurry (<=)) (zip xs (tail xs))

ordinata' :: Ord a => [a] -> Bool
ordinata' xs = and (zipWith (<=) xs (tail xs))

nonord :: Ord a => [a] -> Bool
nonord xs = if (ordinata xs) then False else True

duplicati :: Eq a => [a] -> Bool
duplicati xs = any aux1 xs
        where
            aux1 x = length (filter (== x) xs) > 1

concat1 :: [a] -> [a] -> [a]
concat1 = flip (foldr (:))

concat2 :: [[a]] -> [a]
concat2 = foldr (++) []

rev :: [a] -> [a]
rev = foldl ( flip (:) ) []

cong :: [Bool] -> Bool
cong = foldr (&&) True

dis :: [Bool] -> Bool
dis = foldr (||) False

dis1 :: (Eq a, Integral a) => [a] -> [a]
dis1 = filter (\x -> x `mod` 2 /= 0)

ultimo :: [Int] -> Int
ultimo [x] = x
ultimo (_ : xs) = ultimo xs

polinomio :: [Float] -> Float -> Float
polinomio xs y = sum (map (\(x,v) -> x * v) (zip xs (map (y ^^ ) [0..])))

-- fac simile 1

fc :: [a] -> [a]
fc xs = map fst (filter (even . snd) (zip xs [0..]))

fc1 :: [a] -> [a]
fc1 xs = aux xs 0
    where
        aux [] _ = []
        aux (x : xs) n | n `mod` 2 == 0 = x : aux xs (n+1)
                       | otherwise = aux xs (n+1)

-- fac simile 2

inversioni :: Ord a => [a] -> Int
inversioni xs = length (filter (uncurry (>)) (zip xs (tail xs)))

inversioni1 :: Ord a => [a] -> Int
inversioni1 [] = 0
inversioni1 [_] = 0
inversioni1 (x : y : xs) | x > y = 1 + inversioni1 (y : xs)
                         | otherwise = inversioni1 (y : xs)

-- fac simile 3

ls :: [String] -> String
ls [] = []
ls [x] = []
ls (x : (y : xs)) | (length x) == (length y) = x ++ ls xs
                  | otherwise = ls xs

ls1 :: [String] -> String
ls1 xs = foldr (++) [] (map fst (filter (\(x,y) -> (length x) == (length y)) (zip xs (tail xs))))

-- fac simile 1 rifatto

ep :: [a] -> [a]
ep xs = aiut xs 0
    where
        aiut [] _ = []
        aiut (x : xs) n | n `mod` 2 == 0 = x : aiut xs (n + 1)
                        | otherwise = aiut xs (n + 1)

ep1 :: [a] -> [a]
ep1 xs =  map snd (filter (\(x,y) -> x `mod` 2 == 0) (zip [0..] xs))

-- fac simile 2 rifatto

io :: Ord a => [a] -> Int
io xs = length (map fst (filter (\(x,y) -> x > y) (zip xs (tail xs))))

pol :: [Float] -> Float -> Float
pol xs y =  sum (map (\(x,z) -> x * z) (zip xs (map (y ^^ ) [0..])))

ult :: [Int] -> Int
ult xs = head (map fst (filter (\(x,y) -> y == (((length xs) - 1))) (zip xs [0..])))

dup :: (Eq a, Num a) => [a] -> Bool
dup xs = if (sum (map snd (filter (\(x,y) -> x == y) (zip xs (tail xs))))) == 0 then False else True

mm :: [Float] -> [Float]
mm = map ((*2) . ( + 1))