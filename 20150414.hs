-- #### Trabalho 07 ####

-- Questão 01

compose :: (Eq t) => (t -> t) -> [(t -> t)] -> [(t -> t)]
compose g [] = []
compose g (f:fs) = (g . f) : compose g fs


-- Questão 02

-- {
-- #### Codigo do trabalho 06 para a questão 2 ####
-- Tipos auxiliares: Vertex (representa um vértice do grafo) e Edge (representa uma aresta com seu respectivo peso)
type Vertex t = t
type Edge t = (Vertex t, Vertex t, Int) -- Vértices adjacentes + Peso da aresta

data Graph t = Graph [Vertex t] [Edge t] deriving (Eq, Ord, Show)

myGraph :: Graph Int
myGraph = Graph [1, 2, 3, 4, 5, 6, 7, 8] [(1, 2, 1), (1, 3, 1), (3, 4, 1), (3, 5, 1), (6, 7, 1), (6, 8, 1)]	
-- }

mapGraph :: (Eq t) => (t -> t) -> Graph t -> Graph t
mapGraph f (Graph v adj) = Graph (map f v) adj 

foldGraph :: (Eq t) => (t -> t -> t) -> Graph t -> t 
foldGraph f (Graph v adj) = foldr1 f v 


-- Questão 03
data Tree t = NilT |
			  Node t (Tree t) (Tree t) deriving (Eq, Show)

auxFilter :: (Eq t) => (t -> Bool) -> Tree t -> [Tree t]
auxFilter f NilT = [NilT]
auxFilter f (Node x lft rgt) = [new_return] ++ forest_lft ++ forest_rgt
	where
	all_lft = auxFilter f lft
	all_rgt = auxFilter f rgt
	
	new_lft = head(all_lft)
	new_rgt = head(all_rgt)
	
	forest_lft = if (f x) then tail(all_lft) else all_lft
	forest_rgt = if (f x) then tail(all_rgt) else all_rgt
	
	new_return = if (f x) then (Node x new_lft new_rgt) else NilT
	
filterTree :: (Eq t) => (t -> Bool) -> Tree t -> [Tree t]
filterTree f tree = forest_clr
	where
	forest_dirty = auxFilter f tree
	forest_clr = [value | value <- forest_dirty, not(value == NilT)]

-- #### Exercicios de aula ####