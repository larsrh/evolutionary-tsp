import Evolutionary.Parser
import Evolutionary.Data
import Evolutionary.Algorithm
import Data.List


main :: IO ()
main = do
	doc <- parseInput
	let parsed = fmap (mapToNode . attributesToMap) $ extractNodeAttrs doc
	stuffed <- doStuff parsed
	putStrLn $ show stuffed
