import Data.Maybe
import Control.Monad

type Edge = (Char, Char)
type Graph = [Edge]


grafo = [('a', 'b'), ('b', 'c')]



countEdges :: Graph -> Integer
countEdges [] = 0
countEdges ((a,b):xs) = 1 + countEdges xs


main = do
  let re = countEdges grafo
  print re

