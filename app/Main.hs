{-# LANGUAGE OverloadedStrings #-}
module Main where

import Grafo
import Network.Wai.Middleware.Cors
import Web.Scotty
import Data.List  

graph = [('a', 'b'), ('b', 'c')]

routes :: ScottyM ()
routes = do
  middleware simpleCors
  get "/hello" $ do
      text "hello world!"
  get "/edges" $ do
      let numberEdges = countEdges graph
      json numberEdges
  get "/vertex" $ do
      let numberVertex = countVertices graph
      json numberVertex
  get "/degrees" $ do
      let verticesList = (compress (removeWhiteSpaces (tuplesToString graph)))
      let degreesVertices = calculatingDegrees graph
      let degreeZeroVerticesList = degreeZeroVertices (verticesWithDegree degreesVertices) verticesList
      let allDegreesVerticesList = degreesVertices ++ degreeZeroVerticesList
      json allDegreesVerticesList
  get "/result" $ do
      let verticesList = (compress (removeWhiteSpaces (tuplesToString graph)))
      let degreesVertices = calculatingDegrees graph
      let degreeZeroVerticesList = degreeZeroVertices (verticesWithDegree degreesVertices) verticesList
      let allDegreesVerticesList = degreesVertices ++ degreeZeroVerticesList
      json typeGraph allDegreesVerticesList graph isConnected


main = scotty 3000 routes
