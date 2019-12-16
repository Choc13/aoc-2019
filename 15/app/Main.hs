module Main where

import qualified Data.Map                      as Map
import           Data.List
import           Data.List.Split
import           Lib hiding(read)
import Data.Ord (comparing)
import Data.Maybe

main :: IO ()
main = do
    input <- readFile "input.txt"
    let programList = map read $ splitOn "," input
    let program     = Map.fromList $ zip [0 ..] programList
    let oxygen = answer1 program
    print oxygen
    -- putStrLn $ unlines $ draw $ createMap $ initialSearchState program Point {x = 0, y = 0}
    print $ answer2 program . fst <$> oxygen

initialSearchState program origin = SearchState { 
    visited = Map.fromList [(origin, (0, initialState program))], 
    frontier = [origin] }

draw :: Map.Map Point (Int, StatusCode) -> [String]
draw m =
    let points = Map.keysSet m
        maxY = y $ maximumBy (comparing y) points
        maxX = x $ maximumBy (comparing x) points
    in [ [ drawTile $ fromMaybe (0, Wall) (Map.lookup Point { x = col, y = row } m) | col <- [0..maxX] ] | row <- [0..maxY] ]

drawTile :: (Int, StatusCode) -> Char
drawTile (distance, statusCode) = case statusCode of
    Wall -> '#'
    Moved -> last $ show distance
    Oxygen -> 'O'
