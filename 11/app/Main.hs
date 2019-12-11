module Main where

import qualified Data.Map                      as Map
import           Data.List
import           Data.List.Split
import Data.Ord (comparing)
import Data.Maybe
import           Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let programList = map read $ splitOn "," input
    let program     = Map.fromList $ zip [0 ..] programList
    draw $ answer2 program

draw :: Drawing -> IO ()
draw drawing = do
    let points = Map.keysSet drawing
    let maxY = y $ maximumBy (comparing y) points
    let maxX = x $ maximumBy (comparing x) points
    let chars = [ 
            [ 
                if fromMaybe 0 (Map.lookup Point { x = x, y = y } drawing) == 0 then ' ' else 'X' 
                | x <- [0..maxX] 
            ] | y <- [0..maxY]]
    putStrLn $ unlines chars
