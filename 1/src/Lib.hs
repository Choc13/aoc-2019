module Lib
    ( module Lib
    ) where

totalFuel :: [Int] -> Int
totalFuel masses = sum $ map fuel masses

fuel :: Int -> Int
fuel mass
    | f < 1 = 0
    | otherwise = f + fuel f
    where f = mass `div` 3 - 2
