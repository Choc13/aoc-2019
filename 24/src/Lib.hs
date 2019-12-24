module Lib
    ( module Lib
    )
where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Map as Map
import qualified Data.Set as Set

data Point = Point { x :: Int, y :: Int }
    deriving (Eq, Ord, Show)
data Tile = Empty | Bug
    deriving (Eq, Ord, Show)
type Layout = Map.Map Point Tile

answer1 :: String -> Maybe Int
answer1 = dup . map biodiversity . simulate . createLayout

answer2 :: String -> Int
answer2 input = 0

simulate :: Layout -> [Layout]
simulate = unfoldr (\seed -> Just(seed, nextLayout seed))

nextLayout :: Layout -> Layout
nextLayout layout = Map.mapWithKey (nextTile layout) layout

nextTile :: Layout -> Point -> Tile -> Tile
nextTile layout point tile = case (tile, adjacentBugs point layout) of
    (Empty, 1) -> Bug
    (Empty, 2) -> Bug
    (Bug, 1) -> Bug
    (_, _) -> Empty

adjacentBugs :: Point -> Layout -> Int
adjacentBugs point layout = length $ filter (== Bug) $ neighbours point layout

neighbours :: Point -> Layout -> [Tile]
neighbours point layout =
    map (\p -> Map.findWithDefault Empty p layout)
        [ point { y = y point - 1 }, point { y = y point + 1 }, point { x = x point - 1 }, point { x = x point + 1 }]

biodiversity :: Layout -> Int
biodiversity layout =
    let width = x (maximumBy (comparing x) $ Map.keys layout) + 1
    in sum 
        $ map (biodiversityForTile width)
        $ Map.keys
        $ Map.filter (== Bug) layout

biodiversityForTile :: Int -> Point -> Int
biodiversityForTile width point = 2 ^ ((y point * width) + x point)

createLayout :: String -> Layout
createLayout input = Map.fromList $ do
    (y, row) <- zip [0 ..] $ lines input
    (x, c  ) <- zip [0 ..] row
    pure (Point { x = x, y = y }, createTile c)

createTile :: Char -> Tile
createTile '#' = Bug
createTile '.' = Empty

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s 
                           then Just x
                           else dup' xs (Set.insert x s)