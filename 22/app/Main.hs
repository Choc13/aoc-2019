module Main where

import           Lib
import Data.List

main :: IO ()
main = do
    f <- readFile "input.txt"
    let shuffles = lines f
    -- print $ shuffled shuffles [0..9]
    print $ answer1 10007 2019 $ lines f

shuffled :: [String] -> [Int] -> [Int]
shuffled shuffles deck = map fst 
    $ sortOn snd 
    $ zip deck 
    $ map (\i -> answer1 (length deck) i shuffles) [0.. (length deck - 1)]