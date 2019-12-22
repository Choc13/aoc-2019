{-# LANGUAGE TupleSections #-}

module Lib
    ( module Lib
    )
where

import           Data.Char
import           Data.List
import           Data.Maybe

data Shuffle = Cut Int | Reverse | DealIncrement Int
    deriving (Eq, Ord, Show)
type DeckSize = Int
type Position = Int

answer1 :: DeckSize -> Position -> [String] -> Position
answer1 deckSize pos input =
    foldl (doShuffle deckSize) pos $ parseInstructions input

answer2 :: [String] -> Int
answer2 input = 0

doShuffle :: DeckSize -> Position -> Shuffle -> Position
doShuffle deckSize position shuffle = case shuffle of
    Reverse -> (deckSize - 1) - position
    Cut n -> (position - n) `mod` deckSize
    DealIncrement n -> (position * n) `mod` deckSize

parseInstructions :: [String] -> [Shuffle]
parseInstructions = map parseInstruction

parseInstruction :: String -> Shuffle
parseInstruction i = case words i of
    ["deal", "into", "new", "stack"] -> Reverse
    ["cut", x]                       -> Cut $ read x
    ["deal", "with", "increment", x] -> DealIncrement $ read x
