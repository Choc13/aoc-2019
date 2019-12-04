module Main where

import Data.Array
import Data.List.Split
import Lib

main :: IO ()
main = do
    f <- readFile "input.txt"
    let input = map read $ splitOn "," f
    let answer = findInputs 0 0 input
    print answer
