module Evolutionary.Data where

import qualified Data.Map as Map
import Data.Monoid

data Node = Node String Float Float deriving (Show, Eq, Ord)

data Edge = Edge Node Node Float deriving (Eq)

type EdgeMap = Map.Map (Node, Node) Edge

instance Show Edge where
	show (Edge (Node n1 _ _) (Node n2 _ _) dist) = n1 ++ " → " ++ n2 ++ " (" ++ (show dist) ++ ")"

distance :: Node -> Node -> Float
distance (Node _ x1 y1) (Node _ x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

mkEdge :: Node -> Node -> Edge
mkEdge n1 n2 = Edge n1 n2 $ distance n1 n2

calculateEdges :: [Node] -> Node -> EdgeMap
calculateEdges xs n = Map.fromList $ map (f n) xs where
	f n1 n2 = ((n1, n2), mkEdge n1 n2)

calculateAllEdges :: [Node] -> EdgeMap
calculateAllEdges xs = mconcat $ map (calculateEdges xs) xs
