
module Main where

import           Data.List
import           Data.List.Split
import qualified Data.Map as Map
import           Data.Maybe
import           Lib

main :: IO ()
main = do
    f <- readFile "input.txt"
    let input = Map.fromList $ map parseLine $ lines f
    print $ solve input

parseLine :: String -> (Element, Formula)
parseLine line = 
        let 
            (lhs:rhs:_) = splitOn " => " line
            (quantity:element:_) = splitOn " " rhs
        in (element, Formula {
                quantity = read quantity, 
                inputs = Map.fromList $ map ((\(q:elem:_) -> (elem, read q)) . splitOn " ") $ splitOn ", " lhs
                })