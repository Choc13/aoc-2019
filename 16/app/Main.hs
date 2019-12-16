
module Main where

import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map as Map
import           Lib

main :: IO ()
main = do
    f <- readFile "input.txt"
    let input    = map digitToInt f
    let l        = 10000 * length input
    let repeated = take l $ cycle input
    print $ answer1 input
    print $ answer2 repeated
