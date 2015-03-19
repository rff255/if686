vendas :: Int -> Int
vendas x
 | (x == 1) = 2
 | (x == 2) = 2
 | (x == 3) = 2
 | otherwise = 5

testEqual :: Int -> Int -> Bool
testEqual a b = (a == b)

sumIfEqual :: Int -> Int -> Int -> Int
sumIfEqual a b x
 | (testEqual a b) = x + 1
 | otherwise = x






findVendas :: Int->Int->Int->Int
findVendas s n cont 
 | n == 0 = sumIfEqual s (vendas n) cont
 | otherwise = (findVendas (s) (n-1) (cont)) + (sumIfEqual (s) (vendas(n)) (0))



























