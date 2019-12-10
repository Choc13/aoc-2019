{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( module Lib
    )
where

import Data.Ord (comparing)
import Data.List
import Data.Fixed (mod')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

data Point = Point { x :: Int, y :: Int }
    deriving (Eq, Show)
data Polar = Polar { r :: Double, theta :: Double }
    deriving (Eq, Show)

maxVisibility :: [Point] -> (Point, Int)
maxVisibility ps = (maximumBy (comparing snd) . map (\p -> (p, visibleFrom p ps))) ps

visibleFrom :: Point -> [Point] -> Int
visibleFrom origin = length . Set.fromList . map theta . polarRelativeTo origin

polarRelativeTo :: Point -> [Point] -> [Polar]
polarRelativeTo origin = map (toPolar . shiftBy origin) . filter (origin /=)

destructionSequence :: Point -> [Point] -> [Point]
destructionSequence origin = concat . transpose . rings origin

rings :: Point -> [Point] -> [[Point]]
rings origin = 
    Map.elems .
    Map.fromListWith (flip (++)) .
    map (\p -> ((theta . snd) p, [fst p])) .
    sortOn (r . snd) .
    map (\p -> (p, (toPolar . shiftBy origin) p)) .
    filter (origin /=)

shiftBy :: Point -> Point -> Point
shiftBy origin point = Point { x = x point - x origin, y = y point - y origin }

toPolar :: Point -> Polar
toPolar p =
    let
        r = sqrt (fromIntegral (x p ^ 2) + fromIntegral (y p ^ 2))
        theta = ((5 * pi / 2) + atan2 (fromIntegral $ y p) (fromIntegral $ x p)) `mod'` (2 * pi)
    in Polar {r, theta}
