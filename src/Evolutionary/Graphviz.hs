module Evolutionary.Graphviz where

import Evolutionary.Data

getNodeLine :: Node -> String
getNodeLine (Node name x y) = "\t" ++ show name ++ "[pos=\"" ++ show x ++ "," ++ show y ++ "!\"];\n"

getConnectionLine :: (Node, Node) -> String
getConnectionLine ((Node id1 _ _), (Node id2 _ _)) = "\t" ++ show id1 ++ " -> " ++ show id2 ++ ";\n"

getGraph :: Path -> String
getGraph (Path {nodes = xs}) = "digraph tsp {\n" ++ ns ++ connections ++ "}" where
	ns = concat $ map getNodeLine xs
	connections = concat $ map getConnectionLine (zip xs ((tail xs) ++ [head xs]))
