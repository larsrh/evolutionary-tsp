module Evolutionary.Algorithm where

import qualified Data.Map as Map
import qualified System.Random as Random
import qualified Data.List as List
import Evolutionary.Data

calculateCost :: EdgeMap -> [Node] -> Float
calculateCost costMap nodes = sum costs where
	costs = map lookup connections
	lookup connection = Map.findWithDefault err connection costMap
	connections = zip nodes (tail nodes)
	err = error "connection not found"

crossOver :: [Node] -> [Node] -> Int -> [Node]
crossOver p1 p2 i = start ++ end where
	start = take i p1
	end = List.intersect p2 (drop i p1)

mutation :: [Node] -> Int -> Int -> [Node]
mutation xs i j = start ++ middle ++ end where
	start = take i xs
	middle = reverse $ take (j-i) $ drop i xs
	end = drop j xs

doStuff :: [Node] -> IO [Node]
doStuff l = do
	i <- Random.getStdRandom $ Random.randomR (0, length l)
	j <- Random.getStdRandom $ Random.randomR (0, length l)
	return $ crossOver l (reverse l) i
	--return $ mutation l (min i j) (max i j)
