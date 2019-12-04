module Main where

import Lib

main :: IO ()
main = do
    f <- readFile "input.txt"
    let masses = map read $ lines f
    print $ totalFuel masses
