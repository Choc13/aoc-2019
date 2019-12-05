module Main where

import Data.Array
import Data.List
import Data.List.Split
import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let program = map read $ splitOn "," input
    run (listArray (0, length program - 1) program) 0
