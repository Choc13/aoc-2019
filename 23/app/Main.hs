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
    answer1 program
    -- runComputer $ (initialState program) {inputs = [40]}
    -- runComputer $ (initialState program) {inputs = [41]}
    -- runComputer $ (initialState program) {inputs = [42]}
    -- runComputer $ (initialState program) {inputs = [43]}
    -- runComputer $ (initialState program) {inputs = [44]}
    -- runComputer $ (initialState program) {inputs = [45]}
    -- runComputer $ (initialState program) {inputs = [46]}
    -- runComputer $ (initialState program) {inputs = [47]}
    -- runComputer $ (initialState program) {inputs = [48]}
    -- runComputer $ (initialState program) {inputs = [49]}
    -- runComputer $ (initialState program) {inputs = [50]}
    -- runComputer $ (initialState program) {inputs = [3]}