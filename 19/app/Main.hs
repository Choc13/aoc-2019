module Main where

import qualified Data.Map                      as Map
import           Data.List
import           Data.List.Split
import           Lib hiding(read)
import Data.Ord (comparing)
import Data.Maybe
import Data.Char

main :: IO ()
main = do
    input <- readFile "input.txt"
    let programList = map read $ splitOn "," input
    let program     = Map.fromList $ zip [0 ..] programList
    print $ answer1 program
    -- print $ answer2 program

printMap :: Map.Map Point Char -> IO ()
printMap = putStrLn . unlines . draw

draw :: Map.Map Point Char -> [String]
draw m =
    let points = Map.keysSet m
        maxY = y $ maximumBy (comparing y) points
        maxX = x $ maximumBy (comparing x) points
    in [ [ fromMaybe '?' $ Map.lookup Point { x = col, y = row } m | col <- [0..maxX] ] | row <- [0..maxY] ]
