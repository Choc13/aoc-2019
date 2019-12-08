module Main where

import           Data.List
import           Data.List.Split
import           Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let width = 25
    let height = 6
    let image = chunksOf width $ foldl1 addLayer $ chunksOf (width * height) input
    putStrLn $ unlines $ map (\c -> if c == '1' then '█' else ' ') <$> image

addLayer :: String -> String -> String
addLayer = zipWith (\cur new -> if cur == '2' then new else cur)