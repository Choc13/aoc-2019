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
    print $ answer1 program
