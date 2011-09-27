module Evolutionary.Data where

import qualified Data.Map as Map
import qualified System.Random as Random
import Data.List
import Data.Monoid

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

degToRad :: Float -> Float
degToRad x = rad where
	deg = fromInteger $ round x
	m = x - deg
	rad = pi * (deg + 5.0 * m / 3.0) / 180.0
	round x = floor $ x + 0.5

latLon :: (Float, Float) -> (Float, Float)
latLon (x, y) = (degToRad x, degToRad y)

distance :: Node -> Node -> Float
distance (Node _ x1 y1) (Node _ x2 y2) = (fromInteger . floor) $ rrr * acos val + 1.0 where
	(lat1, lon1) = latLon (x1, y1)
	(lat2, lon2) = latLon (x2, y2)
	rrr = 6378.388
	q1 = cos $ lon1 - lon2
	q2 = cos $ lat1 - lat2
	q3 = cos $ lat1 + lat2
	val = 0.5 * ((1.0 + q1) * q2 - (1.0 - q1) * q3)

calculateEdges :: [Node] -> Node -> EdgeMap
calculateEdges xs n = Map.fromList $ map (f n) xs where
	f n1 n2 = ((n1, n2), distance n1 n2)

calculateAllEdges :: [Node] -> EdgeMap
calculateAllEdges xs = mconcat $ map (calculateEdges xs) xs

calculateCost :: EdgeMap -> [Node] -> Float
calculateCost costMap ns = sum costs where
	costs = map lookup' connections
	lookup' connection = Map.findWithDefault err connection costMap
	connections = zip ns ((tail ns) ++ [head ns])
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
		let e = xs !! rnd
		choose (e : ys) (n-1)

rand :: Int -> Int -> IO Int
rand lower upper = Random.getStdRandom $ Random.randomR (lower, upper)

randomPermutation :: [a] -> IO [a]
randomPermutation [] = return []
randomPermutation (x : xs) = do
	rnd <- rand 0 $ length xs
	rest <- randomPermutation xs
	let (ys, zs) = splitAt rnd rest
	return $ ys ++ (x : zs)

minUniqueN :: Ord a => Int -> [a] -> [a]
minUniqueN n xs = take n (nub $ sortBy compare xs)

iterateM :: Monad m => (a -> Int -> m a) -> Int -> a -> m a
iterateM f n a = iter a n 0 where
	iter a' 0 count = f a' count
	iter a' n' count = do
		r <- f a' count
		iter r (n' - 1) (count + 1)

