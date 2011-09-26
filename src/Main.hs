import Evolutionary.Parser
import Evolutionary.Data


main :: IO ()
main = do
	doc <- parseInput
	let parsed = fmap (mapToNode . attributesToMap) $ extractNodeAttrs doc
	putStrLn $ show parsed

