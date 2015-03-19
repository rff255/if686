-- Double
double :: [Int] -> [Int]
double list
 | (list == []) = []
 | otherwise  = [((head list) + (head list))] ++ double (tail list)

-- Member
member :: [Int] -> Int -> Bool
member list num
 | (list == [])       = False
 | (head list) == num = True
 | otherwise          = (head list) == num || member (tail list) (num)
 
-- Digits
digits :: String -> String
digits str
 | (str == []) = []
 | fromEnum(head str) > 48 && fromEnum(head str) < 58 = [head str] ++ digits (tail str)
 | otherwise = digits (tail str)
 
-- SumPairs
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs list1 list2 
 | list1 == [] || list2 == [] = []
 | otherwise = [(head list1) + (head list2)] ++ sumPairs (tail list1) (tail list2)