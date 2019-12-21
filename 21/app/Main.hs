module Main where

import qualified Data.Map                      as Map
import qualified Data.Set as Set
import           Data.List
import           Data.List.Split
import           Lib hiding(read)
import Data.Ord (comparing)
import Data.Maybe
import Data.Char

main :: IO ()
main = do
    input <- readFile "input.txt"
    let programList = map read $ splitOn "," input
    let program     = Map.fromList $ zip [0 ..] programList
    runScript program "part2.springscript"