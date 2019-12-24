module Main where

import           Lib
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as Map

main :: IO ()
main = do
    f <- readFile "input.txt"
    -- putStrLn $ unlines $ map draw (simulate $ createLayout f) !! 4
    -- print $ biodiversityForTile 5 Point { x = 0, y = 3 }
    -- print $ biodiversityForTile 5 Point { x = 1, y = 4 }
    -- print $ biodiversity $ createLayout f
    print $ answer1 f

draw :: Layout -> [String]
draw m =
    let points = Map.keysSet m
        maxY   = y $ maximumBy (comparing y) points
        maxX   = x $ maximumBy (comparing x) points
    in  [ [ drawSpace $ Map.findWithDefault Empty Point { x = col, y = row } m
            | col <- [0 .. maxX]
            ]
        | row <- [0 .. maxY]
        ]

drawSpace :: Tile -> Char
drawSpace Empty    = '.'
drawSpace Bug = '#'