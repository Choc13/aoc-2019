module Main where

import           Lib
import Data.List

main :: IO ()
main = do
    f <- readFile "input.txt"
    let shuffles = parseInstructions $ lines f
    -- print $ shuffled shuffles [0..9]
    print $ answer1 $ lines f

shuffled :: [Shuffle] -> [Int] -> [Int]
shuffled shuffles deck = map fst 
    $ sortOn snd 
    $ zip deck 
    $ map (\i -> shuffle (length deck) i shuffles) [0.. (length deck - 1)]