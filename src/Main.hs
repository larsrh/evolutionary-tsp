import Data.List
import Data.Monoid
import System.Console.CmdArgs
import System.Environment

import Evolutionary.Parser
import Evolutionary.Data
import Evolutionary.Algorithm
import Evolutionary.Graphviz

data Arguments = Arguments {
	population :: Int,
	steps :: Int
} deriving (Show, Data, Typeable)

defaults = Arguments {
	population = 50,
	steps = 500
}

main :: IO ()
main = do
	args <- cmdArgs defaults
	nodes <- parseInput
	let costMap = calculateAllEdges nodes
	let step = createNewPopulation costMap nodes (population args)
	let repeated = iterateM step (steps args)
	init <- createPopulation costMap nodes (population args)
	p <- repeated init
	let best = minimum p
	putStrLn $ getGraph best 

