import Data.List
import Data.Monoid

import Evolutionary.Parser
import Evolutionary.Data
import Evolutionary.Algorithm

main :: IO ()
main = do
	nodes <- parseInput
	let costMap = calculateAllEdges nodes
	population <- createPopulation costMap nodes 50
	child <- createChildFromPopulation population
	let shown = intersperse "\n" $ fmap show child
	putStrLn $ mconcat shown

