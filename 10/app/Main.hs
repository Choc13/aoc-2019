{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.List
import           Data.List.Split
import           Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rows = lines input
    let charPoints = concatMap (\(y, row) -> map (\(x, c) -> (Point {x, y}, c)) $ zip [0..] row) $ zip [0..] rows
    let points = map fst $ filter (\(_, c) -> c == '#') charPoints
    let origin = fst $ maxVisibility points
    print $ destructionSequence origin points !! 199

