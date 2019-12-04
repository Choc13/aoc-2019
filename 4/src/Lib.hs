module Lib
    ( module Lib
    )
where

import Data.List

isValid :: String -> Bool
isValid x = doesNotDecrease x && hasAdjacentValues x

doesNotDecrease :: String -> Bool
doesNotDecrease x
    | length x < 2 = True
    | otherwise =
        let (prev:curr:rest) = x
        in (curr >= prev) && doesNotDecrease (curr : rest)

hasAdjacentValues :: String -> Bool
hasAdjacentValues x = 2 `elem` sequenceLengths x

sequenceLengths :: String -> [Int]
sequenceLengths x = map length (matchingSequences x)

matchingSequences :: String -> [String]
matchingSequences = reverse . foldl buildSegments [[]]
    
buildSegments :: [String] -> Char -> [String]
buildSegments [[]] next = [[next]]
buildSegments segments next
    | next == head (head segments) = (next : head segments) : tail segments
    | otherwise = [next] : segments