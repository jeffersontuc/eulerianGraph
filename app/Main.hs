{-# LANGUAGE OverloadedStrings #-}
module Main where

import Grafo
import Network.Wai.Middleware.Cors
import Web.Scotty
import Data.List  

graph = [('a', 'b')]

routes :: ScottyM ()
routes = do
  middleware simpleCors
  get "/hello" $ do
      text "hello world!"
  get "/" $ do
      let numberEdges = countEdges graph
      let numberVertices = countVertices graph
      
      json numberEdges


main = scotty 3000 routes
