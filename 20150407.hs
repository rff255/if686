-- Area
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area(Circle r) = 3.1415*(r*r)
area(Rectangle l1 l2) = l1*l2

-- Verifica se eh fim de semana
data Dias = Segunda Int [String] | Terca Int [String] | Quarta Int [String] | Quinta  Int [String] | Sexta Int [String] | Sabado | Domingo 

ehFimDeSemana :: Dias -> Bool
ehFimDeSemana Sabado = True
ehFimDeSemana Domingo = True
ehFimDeSemana _ = False

-- Verifica se tem aula de PLC no dia passado
temPLC :: [String] -> Bool
temPLC [] = False
temPLC (a:as)
 | (a == "PLC") = True
 | otherwise    = temPLC as

temAulaDePLC :: Dias -> Bool
temAulaDePLC Sabado = False
temAulaDePLC Domingo = False
temAulaDePLC (Segunda h aulas) = temPLC aulas
temAulaDePLC (Terca h aulas) = temPLC aulas
temAulaDePLC (Quarta h aulas) = temPLC aulas
temAulaDePLC (Quinta h aulas) = temPLC aulas
temAulaDePLC (Sexta h aulas) = temPLC aulas

-- Modificar o tipo tree para ser comparavel e transformavel em string
data Tree t = Nil | Node t (Tree t) (Tree t) deriving(Eq, Show)

-- Definicao do conjunto de funcoes pedidas
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add expr1 expr2) = "(" ++ (showExpr expr1) ++ "+" ++ (showExpr expr2) ++ ")" 
showExpr (Sub expr1 expr2) = "(" ++ (showExpr expr1) ++ "-" ++ (showExpr expr2) ++ ")" 

--toList 








