module Evolutionary.Data where

import qualified Data.Map as Map
import qualified System.Random as Random
import Data.List
import Data.Monoid
import Control.Monad

data Node = Node String Float Float deriving (Show, Eq, Ord)

type EdgeMap = Map.Map (Node, Node) Float

data Path = Path {
	nodes :: [Node],
	cost :: Float
} deriving (Eq)

instance Ord Path where
	compare xs ys =	if o == EQ
		then compare (nodes xs) (nodes ys)
		else o where
		o = compare (cost xs) (cost ys) 

instance Show Path where
	show path = mconcat (intersperse "→" (map getName $ nodes path)) ++ " : " ++ show (cost path) where 
		getName (Node name _ _) = name

distance :: Node -> Node -> Float
distance (Node _ x1 y1) (Node _ x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

calculateEdges :: [Node] -> Node -> EdgeMap
calculateEdges xs n = Map.fromList $ map (f n) xs where
	f n1 n2 = ((n1, n2), distance n1 n2)

calculateAllEdges :: [Node] -> EdgeMap
calculateAllEdges xs = mconcat $ map (calculateEdges xs) xs

calculateCost :: EdgeMap -> [Node] -> Float
calculateCost costMap nodes = sum costs where
	costs = map lookup connections
	lookup connection = Map.findWithDefault err connection costMap
	connections = zip nodes ((tail nodes) ++ [head nodes])
	err = error "connection not found"

mkPath :: EdgeMap -> [Node] -> Path
mkPath costMap xs = Path {
	nodes = xs,
	cost = calculateCost costMap xs
}

pickRandomBest :: Ord a => Int -> [a] -> IO a
pickRandomBest num xs = do
	picked <- choose [] num
	return $ minimum picked where
	len = length xs
	choose ys 0 = return ys
	choose ys n = do
		rnd <- rand 0 (len-1)
		let elem = xs !! rnd
		choose (elem : ys) (n-1)

rand :: Int -> Int -> IO Int
rand min max = Random.getStdRandom $ Random.randomR (min, max)

randomPermutation :: [a] -> IO [a]
randomPermutation [] = return []
randomPermutation (x : xs) = do
	rand <- rand 0 $ length xs
	rest <- randomPermutation xs
	let (ys, zs) = splitAt rand rest
	return $ ys ++ (x : zs)

minUniqueN :: Ord a => Int -> [a] -> [a]
minUniqueN n xs = take n (nub $ sortBy compare xs)

iterateM :: Monad m => (a -> Int -> m a) -> Int -> a -> m a
iterateM f n a = iter a n 0 where
	iter a 0 count = f a count
	iter a n count = do
		r <- f a count
		iter r (n-1) (count+1)

