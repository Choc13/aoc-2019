
module Main where

import           Data.List
import           Data.List.Split
import Data.Maybe
import           Lib

main :: IO ()
main = do
    let testMoons =
            [ initialMoon [-1, 0, 2]
            , initialMoon [2, -10, -7]
            , initialMoon [4, -8, 8]
            , initialMoon [3, 5, -1]
            ]
    let testMoons2 =
            [ initialMoon [-8, -10, 0]
            , initialMoon [5, 5, 10]
            , initialMoon [2, -7, 3]
            , initialMoon [9, 8, -3]
            ]
    let realMoons =
            [ initialMoon [3, 2, -6]
            , initialMoon [-13, 18, 10]
            , initialMoon [-8, -1, 13]
            , initialMoon [5, 10, 4]
            ]
    print $ foldl (\acc dim -> acc `lcm` fromMaybe 0 (phase dim realMoons)) 1 [0..2]

initialMoon :: [Int] -> Moon
initialMoon pos = Moon { pos = pos, vel = [0, 0, 0] }
