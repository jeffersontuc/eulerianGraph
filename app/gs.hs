import Data.Maybe
import Control.Monad

type Edge = (Char, Char)
type Graph = [Edge]


countEdges :: Graph -> Int
countEdges [] = 0
countEdges (x:[]) = 1
countEdges (x:xs) = 1 + countEdges xs

countVertices :: Graph -> Int
countVertices [] = 0
countVertices (x:[]) = 2
countVertices (x:xs) = length( compress (tupleToList (x:xs)) )


tupleToList :: [(a, a)] -> [a]
tupleToList ([]) = [] 
tupleToList (x:[]) = (fst x):(snd x):[]
tupleToList (x:xs) = (fst x):(snd x):(tupleToList xs)

{-
- Remove os elementos duplicados de uma lista. Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
-}
compress :: Eq t => [t] -> [t]
compress [] = []
compress (x:xs) | (elem x xs) = compress (x:(remove x xs))
                | otherwise = x:(compress xs)

remove :: Eq a => a -> [a] -> [a]
remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)

removeAll :: Eq t => t -> [t] -> [t]
removeAll e [] = []
removeAll e (x:xs) | e == x = removeAll e xs
                   | otherwise = x:(removeAll e xs)

{-
- Retorna uma lista de pares com os elementos e suas quantidades. Ex: encode [2,2,2,3,4,2,5,2,4,5] = [(2,5),(3,1),(4,2),(5,2)]
-}
encode xs = compress ([(a,b) | a <- xs, b <- [0..(length xs)], b==(length (filter (a==) xs))])
encode' xs = encode (tupleToList xs)
{-
- LEMBRAR - Um grafo G CONEXO possui caminho euleriano se e somente se ele tem exatamente ZERO ou DOIS vértices de grau impar. 
-}

degreeList [] = []
degreeList (x:xs) = (snd x):(degreeList xs)

typeGraph xs | and (map even xs) = putStrLn ("It's Eulerian cycle graph")
             | even (length(filter odd xs)) = putStrLn ("It's Semi-Eulerian graph")
             | otherwise = putStrLn ("It's not Eulerian graph")

-------------------------------------------------
grafo = [('a', 'b'), ('b', 'c')] -- exemplo teste
-------------------------------------------------

main :: IO ()
main = do
  print "Inform the graph: (Ex: [('a', 'b'), ('b', 'c')])"
  input <- getLine
  let graph = read input
  let numberEdges = countEdges graph
  let numberVertices = countVertices graph
  let degreesVertices = encode' graph
  let result = typeGraph (degreeList (encode' graph))
  putStrLn ("This graph have " ++ (show(numberEdges)) ++ " edge/es.")
  putStrLn ("This graph have " ++ (show(numberVertices)) ++ " vertixe/ces.")
  putStrLn ("Each vertixe this graph, have the following degree/es: " ++ (show(degreesVertices)))
  result