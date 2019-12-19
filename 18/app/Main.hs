module Main where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.List
import           Data.List.Split
import           Lib                     hiding ( read )
import           Data.Ord                       ( comparing )
import           Data.Maybe

main :: IO ()
main = do
    f <- readFile "input.txt"
    let cave                       = createCave f
    let [entrance]                 = Map.keys $ Map.filter (== '@') cave
    let candidates = answer1 f
    let maxChain = maximum $ map (\(KeyChain chain, _) -> length chain) candidates
    print $ minimumBy (comparing snd) $ filter (\(KeyChain chain, distance) -> length chain == maxChain) candidates

draw :: Cave -> [String]
draw m =
    let points = Map.keysSet m
        maxY   = y $ maximumBy (comparing y) points
        maxX   = x $ maximumBy (comparing x) points
    in  [ [ Map.findWithDefault '#' Point { x = col, y = row } m
          | col <- [0 .. maxX]
          ]
        | row <- [0 .. maxY]
        ]
