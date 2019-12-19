module Main where

import qualified Data.Map                      as Map
import qualified Data.Set as Set
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
    -- print $ answer1 program
    print $ answer2 program

printMap :: Set.Set Point -> IO ()
printMap = putStrLn . unlines . draw

draw :: Set.Set Point -> [String]
draw m =
    [ [ if Set.member Point { x = col, y = row } m then '#' else '.' | col <- [0..49] ] | row <- [0..49] ]
