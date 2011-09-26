module Evolutionary.Data where

import qualified Data.Map as Map
import Data.Monoid

data Node = Node String Float Float deriving (Show, Eq, Ord)

type EdgeMap = Map.Map (Node, Node) Float

distance :: Node -> Node -> Float
distance (Node _ x1 y1) (Node _ x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

calculateEdges :: [Node] -> Node -> EdgeMap
calculateEdges xs n = Map.fromList $ map (f n) xs where
	f n1 n2 = ((n1, n2), distance n1 n2)

calculateAllEdges :: [Node] -> EdgeMap
calculateAllEdges xs = mconcat $ map (calculateEdges xs) xs
