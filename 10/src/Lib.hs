module Lib
    ( module Lib
    )
where

import Data.Ord (comparing)
import Data.List
import Data.List.Split
import Data.Fixed (mod')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Numeric.MathFunctions.Comparison (within)

type Point = (Int, Int)
type Polar = (Double, Double)

maxVisibility :: [Point] -> (Point, Int)
maxVisibility ps = (maximumBy (comparing snd) . map (\p -> (p, visibleFrom p ps))) ps

visibleFrom :: Point -> [Point] -> Int
visibleFrom origin = length . Set.fromList . map snd . polarRelativeTo origin

polarRelativeTo :: Point -> [Point] -> [Polar]
polarRelativeTo origin = map (toPolar . shiftBy origin) . filter (origin /=)

destructionSequence :: Point -> [Point] -> [Point]
destructionSequence origin = concat . transpose . rings origin

rings :: Point -> [Point] -> [[Point]]
rings origin = 
    Map.elems .
    Map.fromListWith (flip (++)) .
    map (\p -> ((snd . snd) p, [fst p])) .
    sortOn (fst . snd) .
    map (\p -> (p, (toPolar . shiftBy origin) p)) .
    filter (origin /=)

shiftBy :: Point -> Point -> Point
shiftBy amount point = (fst point - fst amount, snd point - snd amount)

toPolar :: Point -> Polar
toPolar p =
    let
        r = sqrt (fromIntegral (fst p ^ 2) + fromIntegral (snd p ^ 2))
        theta = ((5 * pi / 2) + atan2 (fromIntegral (snd p)) (fromIntegral (fst p))) `mod'` (2 * pi)
    in (r, theta)
