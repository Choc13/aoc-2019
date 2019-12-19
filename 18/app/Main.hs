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
    let keyGraph = createKeyGraph cave entrance
    -- putStrLn $ unlines $ drawKeyGraph keyGraph
    -- print $ Map.mapWithKey (\k v -> show (length v)) keyGraph
    print $ minimumBy (comparing snd) $ map (\(KeyChain kc, d) -> (kc, d)) $ answer1 f

drawKeyGraph :: KeyGraph -> [String]
drawKeyGraph graph = concat $ Map.mapWithKey drawViewFromKey graph

drawViewFromKey :: Key -> [(Key, Int, KeyChain)] -> [String]
drawViewFromKey key others = (key: "->") : do
    other <- others
    pure ('\t' : show other)

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
