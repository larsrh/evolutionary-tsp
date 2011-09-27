module Evolutionary.Parser where

import Data.List
import Data.Maybe
import Text.XML.HaXml
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Types

import Evolutionary.Data

attributeValues :: Attribute -> [String]
attributeValues (_, (AttValue xs)) = mapMaybe f xs where
	f (Left s) = Just s
	f _ = Nothing

extractNodeAttrs :: Document a -> [[Attribute]]
extractNodeAttrs (Document _ _ (Elem _ _ children) _) = mapMaybe f children where
	f (CElem (Elem name attrs _) _) | name == "node" = Just attrs
	f _ = Nothing

attributesToMap :: [Attribute] -> [(String, String)]
attributesToMap xs = map f xs where
	f :: Attribute -> (String, String)
	f attr = (fst attr, attributeValues attr !! 0)

splitAtChar :: String -> Char -> (String, String)
splitAtChar str c = splitAt index str where
	index = maybe (error "no valid coordinates") id $ findIndex (c ==) str

mapToNode :: [(String, String)] -> Node
mapToNode m = Node name x y where
	name = find "id"
	coords = find "name"
	(sx, sy) = splitAtChar coords ' '
	x = read sx
	y = read sy
	find field = maybe (err field) id $ lookup field m
	err field = error $ "attribute " ++ field ++ " not found"

parseInput :: IO [Node]
parseInput = do
	doc <- fmap (xmlParse "stdin") getContents
	let nodes = fmap (mapToNode . attributesToMap) $ extractNodeAttrs doc
	return nodes

