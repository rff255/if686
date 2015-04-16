-- Trabalho 08
-- Mergesort para auxiliar a questão
mergesort :: (Ord a) => (Eq a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs   =  merge (mergesort (fst (divideInHalf xs))) (mergesort (snd (divideInHalf xs)))

merge :: (Ord a) => (Eq a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
 | x <= y = x : merge (xs) (y:ys)
 | otherwise = y : merge (x:xs) (ys)

divideInHalf :: (Ord a) => (Eq a) => [a] -> ([a], [a]) 
divideInHalf x = (take (length x `div` 2) x, drop (length x `div` 2) x)

-- Funcao auxiliar que faz tudo se tiver todos os parametros.
aux :: (Ord a) => (Num a) => [a] -> [a] -> [[a]]
aux [] l_ = [mergesort l_]
aux l l_ = [curr_part] ++ (aux (tail(l)) others)
    where 
    curr_part = mergesort (filter (<= curr) (l_))
    others = mergesort (filter (> curr) (l_))
    curr   = head(l)

-- Funcao principal
listPartitioner :: (Ord a) => (Num a) => [a] -> ([a] -> [[a]])
listPartitioner l = function
    where 
    function list = aux (mergesort l) list