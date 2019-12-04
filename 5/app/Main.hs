module Main where

import Data.List
import Data.List.Split
import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let answer = input
    print answer
