module Main where

import Data.Array
import Data.List
import Data.List.Split
import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ answer2 $ lines input
