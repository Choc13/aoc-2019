{-# LANGUAGE BangPatterns #-}

module Lib
    ( module Lib
    )
where

import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe

answer1 :: [Int] -> String
answer1 input = outputAfter 100 $ phases phase $ input

answer2 :: [Int] -> String
answer2 input = 
    let offset = read $ concatMap show $ take 7 input
    in outputAfter 100 $ phases phaseLowerHalf $ drop offset input

outputAfter :: Int -> [[Int]] -> String
outputAfter n ps = concatMap show $ take 8 $ ps !! n

phases :: ([Int] -> [Int]) -> [Int] -> [[Int]]
phases createPhase = unfoldr (\next -> Just (next, createPhase next))

phase :: [Int] -> [Int]
phase input = map (\n -> calculateDigit n input) [0 .. (length input - 1)]

phaseLowerHalf :: [Int] -> [Int]
phaseLowerHalf = scanr1 (\acc x -> (acc + x) `mod` 10)

calculateDigit :: Int -> [Int] -> Int
calculateDigit ix input = (abs . sum . zipWith (*) input $ pattern ix) `mod` 10

pattern :: Int -> [Int]
pattern ix =
    (tail . map (\n -> [0, 1, 0, -1] !! ((n `div` (ix + 1)) `mod` 4)))
        [0 ..]
