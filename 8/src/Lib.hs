{-# LANGUAGE RecordWildCards #-}

module Lib
    ( module Lib
    )
where

import Data.Ord (comparing)
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

answer1 :: [Int] -> Int
answer1 input = snd 
    $ minimumBy (comparing fst)
    $ map (\layer -> (digitCount 0 layer, digitCount 1 layer * digitCount 2 layer)) 
    $ createDigitMaps input

digitCount :: Int -> Map.Map Int Int -> Int
digitCount digit layer = fromMaybe 0 $ Map.lookup digit layer

createDigitMaps :: [Int] -> [Map.Map Int Int]
createDigitMaps input = foldl 
    (\maps (ix, digit) -> case ix `mod` (25 * 6) of
        0 -> Map.fromList [(digit, 1)] : maps
        _ -> Map.insertWith (+) digit 1 (head maps) : tail maps
    )
    []
    (zip [0..] input)
