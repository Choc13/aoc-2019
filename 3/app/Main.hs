module Main where

import Data.List
import Data.List.Split
import Lib

main :: IO ()
main = do
    f <- readFile "input.txt"
    let (wire1:wire2:_) = map (splitOn ",") (lines f)
    print $ firstIntersection wire1 wire2