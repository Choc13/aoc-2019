module Main where

import qualified Data.Map                      as Map
import           Data.List
import           Data.List.Split
import           Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let programList = map read $ splitOn "," input
    let program     = Map.fromList $ zip [0 ..] programList
    boost ProgramState { program = program, ip = 0, rb = 0 } 2
