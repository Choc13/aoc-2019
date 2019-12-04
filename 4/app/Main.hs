module Main where

import Data.List
import Data.List.Split
import Lib

main :: IO ()
main = do
    let passwords = map show [240298..784956]
    let answer = length $ filter isValid passwords
    print answer
