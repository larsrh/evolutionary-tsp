import Data.List
import Data.Monoid
import System.Console.CmdArgs
import System.Environment
import IO

import Evolutionary.Parser
import Evolutionary.Data
import Evolutionary.Algorithm
import Evolutionary.Graphviz

data OutputMode = GraphViz | Plain deriving (Show, Eq, Enum, Data, Typeable)

data Arguments = Arguments {
	population :: Int,
	steps :: Int,
	mode :: OutputMode,
	verbose :: Bool
} deriving (Show, Data, Typeable)

defaults = Arguments {
	population = 50,
	steps = 500,
	mode = Plain,
	verbose = False
}

main :: IO ()
main = do
	args <- cmdArgs defaults
	nodes <- parseInput
	let costMap = calculateAllEdges nodes
	let step p = do
		p' <- createNewPopulation costMap nodes (population args) p
		if verbose args
			then putStrLn $ show (minimum p')
			else return ()
		return p'
	let repeated = iterateM step (steps args)
	init <- createPopulation costMap nodes (population args)
	p <- repeated init
	let best = minimum p
	if mode args == GraphViz
		then putStrLn $ getGraph best
		else putStrLn $ show best
