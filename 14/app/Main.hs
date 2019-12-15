
module Main where

import           Data.List
import           Data.List.Split
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Maybe
import           Lib

main :: IO ()
main = do
    f <- readFile "input.txt"
    let input  = Map.fromList $ map parseLine $ lines f
    let maxOre = 1000000000000
    print $ binarySearch maxOre 0 1000000000000 input

parseLine :: String -> (Element, Formula)
parseLine line =
    let (lhs      : rhs     : _) = splitOn " => " line
        (quantity : element : _) = splitOn " " rhs
    in  ( element
        , Formula
            { quantity = read quantity
            , inputs   = Map.fromList
                         $ map ((\(q : elem : _) -> (elem, read q)) . splitOn " ")
                         $ splitOn ", " lhs
            }
        )

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where
    dup' [] _ = Nothing
    dup' (x : xs) s =
        if Set.member x s then Just x else dup' xs (Set.insert x s)
