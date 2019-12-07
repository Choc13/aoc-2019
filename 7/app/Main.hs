module Main where

import           Data.Array
import           Data.List
import           Data.List.Split
import           Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let programList = map read $ splitOn "," input
    let program = listArray (0, length programList - 1) programList
    print $ answer program
