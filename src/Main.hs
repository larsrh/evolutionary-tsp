import System.Console.CmdArgs

import Evolutionary.Parser
import Evolutionary.Data
import Evolutionary.Algorithm
import Evolutionary.Graphviz

data OutputMode = GraphViz | Plain deriving (Show, Eq, Enum, Data, Typeable)

data Arguments = Arguments {
	population :: Int,
	steps :: Int,
	mode :: OutputMode,
	verbose :: Bool,
	mutationRate :: Int
} deriving (Show, Data, Typeable)

defaults :: Arguments
defaults = Arguments {
	population = 50,
	steps = 500,
	mode = Plain,
	verbose = False,
	mutationRate = 1
}

main :: IO ()
main = do
	arguments <- cmdArgs defaults
	nodes <- parseInput
	let costMap = calculateAllEdges nodes
	let step p count = do
		p' <- createNewPopulation costMap nodes (population arguments) (mutationRate arguments) p
		if verbose arguments
			then putStrLn $ show count ++ ":\t" ++ show (minimum p')
			else return ()
		return p'
	let repeated = iterateM step (steps arguments)
	initialPopulation <- createPopulation costMap nodes (population arguments)
	p <- repeated initialPopulation
	let best = minimum p
	if mode arguments == GraphViz
		then putStrLn $ getGraph best
		else putStrLn $ show best
