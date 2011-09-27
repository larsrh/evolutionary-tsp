import Data.List
import Data.Monoid

import Evolutionary.Parser
import Evolutionary.Data
import Evolutionary.Algorithm
import Evolutionary.Graphviz

main :: IO ()
main = do
	nodes <- parseInput
	let costMap = calculateAllEdges nodes
	population <- createPopulation costMap nodes 50
	let step = createNewPopulation costMap nodes 50
	let repeated = iterateM step 500
	p <- repeated population
	let best = minimum p
	putStrLn $ getGraph best 

