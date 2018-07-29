import Data.Maybe
import Control.Monad

type Vertixe = Char
type Edge = (Vertixe, Vertixe)
type Graph = [Edge]

countEdges :: Graph -> Int
countEdges [] = 0
countEdges (x:xs) | ((fst x)==' ' || (snd x)==' ') = countEdges xs
                  | otherwise = 1 + (countEdges xs)

countVertices :: Graph -> Int
countVertices [] = 0
countVertices (x:xs) = length( compress (removeWhiteSpaces (tupleToList (x:xs))) )

isConected :: Graph -> Bool
isConected [] = False
isConected xs = ((countEdges xs) >= ((countVertices xs)-1)) && ((countEdges xs)>0 && (countVertices xs)>0)

removeWhiteSpaces :: String -> String
removeWhiteSpaces = filter (/=' ')

tupleToList :: [(a, a)] -> [a]
tupleToList [] = [] 
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
encode' xs = encode (removeWhiteSpaces (tupleToList xs))
{-
- LEMBRAR - Um grafo G CONEXO possui caminho euleriano se e somente se ele tem exatamente ZERO ou DOIS vértices de grau impar. 
-}

degreeList :: [(a, t)] -> [t]
degreeList [] = []
degreeList (x:xs) = (snd x):(degreeList xs)

typeGraph xs ys | ( isConected ys ) && ( and (map even xs) ) = putStrLn ("It's Eulerian cycle graph")
                | ( isConected ys ) && ( even (length(filter odd xs)) ) = putStrLn ("It's Semi-Eulerian graph")
                | otherwise = putStrLn ("It's not Eulerian graph")

--exemplo teste-----------------------------------------------
grafo1 = [('a', 'b'), ('b', 'c')] 
grafo2 = [('a', ' '), ('b', ' ')] -- ' ' diz que não está conectado 
-------------------------------------------------


main :: IO ()
main = do
  putStrLn ""
  print "Inform the graph: (Ex: [('a', 'b'), ('b', 'c')])"
  input <- getLine
  let graph = read input
  let numberEdges = countEdges graph
  let numberVertices = countVertices graph
  let degreesVertices = encode' graph
  let result = typeGraph (degreeList (encode' graph)) graph
  putStrLn ""
  putStrLn ("Then...")
  putStrLn ""
  putStrLn ("This graph has " ++ (show(numberEdges)) ++ " edge/es.")
  putStrLn ("This graph has " ++ (show(numberVertices)) ++ " vertixe/ces.")
  putStrLn ("Each vertixe this graph, has the following degree/es: " ++ (show(degreesVertices)))
  result