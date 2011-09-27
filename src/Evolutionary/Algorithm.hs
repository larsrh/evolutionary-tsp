module Evolutionary.Algorithm where

import qualified Data.Map as Map
import qualified System.Random as Random
import qualified Data.List as List
import Control.Monad

import Evolutionary.Data

crossOver :: [Node] -> [Node] -> Int -> [Node]
crossOver p1 p2 i = start ++ end where
	start = take i p1
	end = List.intersect p2 (drop i p1)

mutation :: [Node] -> Int -> Int -> [Node]
mutation xs i j = start ++ middle ++ end where
	start = take i xs
	middle = reverse $ take (j-i) $ drop i xs
	end = drop j xs

chooseParents :: [Path] -> IO (Path, Path)
chooseParents xs = do
	p1 <- pickRandomBest 2 xs
	p2 <- pickRandomBest 2 xs
	return (p1, p2)

createChild :: (Path, Path) -> IO [Node]
createChild (p1, p2) = do
	let len = length $ nodes p1
	rnd <- rand 0 len
	let crossedOver = crossOver (nodes p1) (nodes p2) rnd
	i <- rand 0 len
	j <- rand 0 len
	rnd <- rand 0 100
	return $ if rnd == 0
		then mutation crossedOver (min i j) (max i j)
		else crossedOver

createChildFromPopulation :: [Path] -> IO [Node]
createChildFromPopulation = chooseParents >=> createChild

createPopulation :: EdgeMap -> [Node] -> Int -> IO [Path]
createPopulation costMap nodes = create [] where
	create xs 0 = return xs
	create xs n = do
		perm <- randomPermutation nodes
		let path = mkPath costMap perm
		create (path : xs) (n - 1)

createChildrenFromPopulation :: EdgeMap -> Int -> [Path] -> IO [Path]
createChildrenFromPopulation costMap n population = sequence children where
	children = replicate n child
	child = do
		nodes <- createChildFromPopulation population
		return $ mkPath costMap nodes

createNewPopulation :: EdgeMap -> [Node] -> Int -> [Path] -> IO [Path]
createNewPopulation costMap nodes n population = do
	children <- createChildrenFromPopulation costMap n population
	let total = population ++ children
	let len = length population
	let best = minUniqueN len total
	let newLen = length best
	add <- if newLen < len
		then createPopulation costMap nodes (newLen - len) 
		else return []
	return $ best ++ add

