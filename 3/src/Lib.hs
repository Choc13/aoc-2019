module Lib
    ( module Lib
    )
where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

data Point = Point { x :: Int, y :: Int }
        deriving (Show, Eq, Ord)

closestIntersection :: [String] -> [String] -> Int
closestIntersection wire1 wire2 =
    let intersections = Set.fromList (generatePath wire1) `Set.intersection` Set.fromList (generatePath wire2)
    in head $ tail $ Set.elems $ Set.map distanceFromOrigin intersections

distanceBetween :: Point -> Point -> Int
distanceBetween Point { x = x1, y = y1 } Point { x = x2, y = y2 } =
    abs (x2 - x1) + abs (y2 - y1)

distanceFromOrigin :: Point -> Int
distanceFromOrigin = distanceBetween Point {x = 0, y = 0}

firstIntersection :: [String] -> [String] -> Int
firstIntersection wire1 wire2 = head $ tail $ sort $ Map.elems $ Map.intersectionWith (+) (firstVisits wire1) (firstVisits wire2)

firstVisits :: [String] -> Map.Map Point Int
firstVisits input = Map.fromListWithKey (\_ __ existing -> existing) (zip (reverse $ generatePath input) [0..])

generatePath :: [String] -> [Point]
generatePath =
    foldl (\path instruction -> generateSegment (head path) instruction ++ path) [Point { x = 0, y = 0 }]

generateSegment :: Point -> String -> [Point]
generateSegment Point { x = startX, y = startY } (direction : distance)
    | direction == 'U'
        = [Point {x=x, y=y} | x <- [startX], y <- [startY + size, startY + size - 1 .. startY + 1]]
    | direction == 'D'
        = [Point {x=x, y=y} | x <- [startX], y <- [startY - size .. startY - 1]]
    | direction == 'L'
        = [Point {x=x, y=y} | x <- [startX - size .. startX - 1], y <- [startY]]
    | direction == 'R'
        = [Point {x=x, y=y} | x <- [startX + size, startX + size - 1 .. startX + 1], y <- [startY]]
    | otherwise
        = error [direction]
    where
        size = read distance
