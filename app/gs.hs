import Data.Maybe
import Data.List
import Control.Monad

type Vertex = Char
type Edge = (Vertex, Vertex)
type Graph = [Edge]

{-
-- Função auxiliar de countEdges
-}
countEdges' :: Graph -> Int
countEdges' [] = 0
countEdges' (x:xs) | ((fst x)==' ' || (snd x)==' ' || ((snd x)==(fst x))) = countEdges xs
                  | otherwise = 1 + (countEdges xs)

countEdges :: Graph -> Int
countEdges xs = countEdges' (compress (sortTuplesInList xs))
-------------------------------

countVertices :: Graph -> Int
countVertices [] = 0
countVertices (x:xs) = length( compress (removeWhiteSpaces (tuplesToString ( compress (x:xs) ))) )

{-
-- Ordena caracteres entre tuplas.
-}
sortTuple :: Ord a => (a,a) -> (a,a)
sortTuple (x,y) | x>y = (y,x)
                | otherwise = (x,y)
{-
-- Ordena caracteres entre tuplas de uma lista de tuplas.
-}
sortTuplesInList :: Ord a => [(a,a)] -> [(a,a)]
sortTuplesInList xs = map sortTuple xs

{-
-- Remove arestas para calcular as arestas. 
-- Ex: removeEdgesNotConnected [('a', 'a'), (' ','b'),(' ',' ')] = []
-}
removeEdgesNotConnected :: Graph -> Graph
removeEdgesNotConnected [] = []
removeEdgesNotConnected (x:xs) | ((fst x)==' ' || ((fst x)==(snd x))) = removeEdgesNotConnected xs
                           | otherwise = x:(removeEdgesNotConnected xs)

{-
-- Remove espaço de Strings (ou lista de caracteres).
-- Ex: removeWhiteSpaces "abd e" = "abde"
-}
removeWhiteSpaces :: String -> String
removeWhiteSpaces = filter (/=' ')

{-
-- Remove a primeira ocorrência do elemento "e" de uma lista. 
-- Ex: remove 8 [2,5,8,2,1,8] = [2,5,2,1,8]
-}
remove :: Eq a => a -> [a] -> [a]
remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)

{-
-- Remove os elementos duplicados de uma lista. 
-- Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
-}
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) | (elem x xs) = compress (x:(remove x xs))
                | otherwise = x:(compress xs)

{-
-- Retorna uma lista de pares com os elementos e suas quantidades. 
-- Ex: encode "asdaaaaaa" = [('a',7),('s',1),('d',1)]
-}
encode :: Eq a => [a] -> [(a, Int)]
encode xs = compress ( [(a,b) | a <- xs, b <- [0..(length xs)], b==(length (filter (a==) xs))] )

{-
-- Calcula graus dos vertices não nulos, renornando o Vertice e seu respectivo grau. 
-- Ex: calculatingDegrees [('a', 'b'), ('b', 'c')] = [('a',1),('b',2),('c',1)]
-}
calculatingDegrees :: Graph -> [(Vertex, Int)]
calculatingDegrees xs = encode (removeWhiteSpaces (tuplesToString ( removeEdgesNotConnected (compress (sortTuplesInList xs)) )))

{-
-- Converte uma lista de tuplas com graus dos vertices, para uma lista de inteiros dos graus. 
-- Ex: degreesList [('a',1),('b',2),('c',1)] = [1,2,1]
-}
degreesList :: [(Vertex, Int)] -> [Int]
degreesList [] = []
degreesList (x:xs) = (snd x):(degreesList xs)

{-
-- Converte uma lista de tuplas para uma lista de caracteres. 
-- Ex: tuplesToString [('a', 'b'), ('b', 'c')] = "abbc"
-}
tuplesToString :: [(a, a)] -> [a]
tuplesToString [] = [] 
tuplesToString (x:[]) = (fst x):(snd x):[]
tuplesToString (x:xs) = (fst x):(snd x):(tuplesToString xs)

{-
-- Função auxiliar de typeGraph, que verifica e imprime se o grafo não é euleriano, é euleriano ou semi-euleriano 
-}
typeGraph' :: Integral a => [a] -> Graph -> IO ()
typeGraph' xs ys | ( isConnected ys ) && ( and (map even xs) ) = putStrLn ("It's Eulerian cycle graph")
                | ( isConnected ys ) && ( even (length(filter odd xs)) ) = putStrLn ("It's Semi-Eulerian graph")
                | otherwise = putStrLn ("It's not Eulerian graph")

typeGraph :: [(Vertex, Int)] -> Graph -> IO ()
typeGraph xs ys = typeGraph' (degreesList xs) ys
-----------------------------------------------------------------------------------------------------------------

{-
-- Retorna a lista de vertices que calculou-se grau. 
-- Ex: verticesWithDegree [('a',1),('b',1)] = "ab"
-}
verticesWithDegree :: [(Vertex, Int)] -> [Vertex]
verticesWithDegree [] = []
verticesWithDegree (x:xs) = (fst x):(verticesWithDegree xs)

{-
-- Retorna apenas a lista de tuplas com vertices de grau zero, recebendo a lista de vertices com grau maior que zero e a lista dos vertices total.
-}
degreeZeroVertices :: (Eq t1, Num t2, Foldable t) => t t1 -> [t1] -> [(t1, t2)]
degreeZeroVertices xs [] = []
degreeZeroVertices xs (y:ys) | elem y xs = degreeZeroVertices xs ys
                             | otherwise = ( (y,0) ):(degreeZeroVertices xs ys)

{-
-- Retorna se o grafo é connectado ou não.
-}
{-
--isConnected :: Graph -> Bool
--isConnected [] = False
--isConnected xs = ((countEdges xs) >= ((countVertices xs)-1)) && ((countEdges xs)>0 && (countVertices xs)>0)
-}
g a ([]) ([]) = True
g a (x:xs) ([]) = True
g a ([]) (y:ys) = False
g a (x:xs) (y:ys) | (a/=(snd x)) = g (snd x) xs ys
                  | otherwise = g (snd x) (xs) (ys)
main :: IO ()
main = do
  -- Recebendo o grafo
  putStrLn ""
  print "Inform the graph: (Ex: [('a', 'b'), ('b', 'c')])"
  input <- getLine
  let graph = read input
  
  -- Calculando dados sobre o grafo
  let numberEdges = countEdges graph
  let numberVertices = countVertices graph

  let verticesList = (compress (removeWhiteSpaces (tuplesToString graph)))

  let degreesVertices = calculatingDegrees graph
  let degreeZeroVerticesList = degreeZeroVertices (verticesWithDegree degreesVertices) verticesList
  let allDegreesVerticesList = degreesVertices ++ degreeZeroVerticesList

  --let t = g (fst (head (sort (sortTuplesInList (compress graph)))) ) (sort (sortTuplesInList (compress graph))) verticesList
  let t = g (fst (head ((sortTuplesInList ( graph)))) ) ((sortTuplesInList ( graph))) verticesList
  -- Imprimindo os resultados
  putStrLn ""
  putStrLn ("Then...")
  putStrLn ""
  putStrLn ("This graph has " ++ (show numberEdges) ++ " edges.")
  putStrLn ("This graph has " ++ (show numberVertices) ++ " vertices.")
  putStrLn ("Each vertex this graph, has the following degrees: " ++ (show allDegreesVerticesList))
  typeGraph allDegreesVerticesList graph

  print ( (verticesList/="") && (degreeZeroVerticesList==[]) && t )
  