module Main where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.List
import           Data.List.Split
import           Lib                     hiding ( read )
import           Data.Ord                       ( comparing )
import           Data.Maybe

main :: IO ()
main = do
    f <- readFile "input.txt"
    putStrLn $ unlines $ draw $ createMaze f
    print $ answer1 f

draw :: Maze -> [String]
draw m =
    let points = Map.keysSet m
        maxY   = y $ maximumBy (comparing y) points
        maxX   = x $ maximumBy (comparing x) points
    in  [ [ Map.findWithDefault ' ' Point { x = col, y = row } $ Map.map drawSpace m
          | col <- [0 .. maxX]
          ]
        | row <- [0 .. maxY]
        ]

drawSpace :: Space -> Char
drawSpace Empty    = '.'
drawSpace (Portal p) = head p
