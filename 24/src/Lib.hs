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
data Tile = Empty | Bug | Centre
    deriving (Eq, Ord, Show)
type Layout = Map.Map Point Tile
type Layers = Map.Map Int Layout

answer1 :: String -> Maybe Int
answer1 = dup . map biodiversity . simulate . createLayout

answer2 :: Int -> String -> Int
answer2 time = countBugs . last . take (time + 1) . simulateRecursive . createLayers

simulateRecursive :: Layers -> [Layers]
simulateRecursive = unfoldr (\seed -> Just(seed, nextLayers seed))

simulate :: Layout -> [Layout]
simulate = unfoldr (\seed -> Just(seed, nextLayout seed))

nextLayers :: Layers -> Layers
nextLayers layers = 
    Map.mapWithKey (\l layout -> 
            Map.mapWithKey (\p v -> nextTile (adjacentRecursiveBugs l p layers) v) layout
        ) $ addLayers layers

addLayers :: Layers -> Layers
addLayers layers =
    let levels = Map.keysSet layers
        minLevel = Set.findMin levels
        maxLevel = Set.findMax levels
    in Map.insert (minLevel - 1) (emptyLayer 5) $ Map.insert (maxLevel + 1) (emptyLayer 5) layers

nextLayout :: Layout -> Layout
nextLayout layout = Map.mapWithKey (\p v -> nextTile (adjacentBugs p layout) v) layout

adjacentRecursiveBugs :: Int -> Point -> Layers -> Int
adjacentRecursiveBugs level point layers = 
    length $ filter (== Bug) $ recursiveNeighbours level point layers

adjacentBugs :: Point -> Layout -> Int
adjacentBugs point layout =
    let neighbours = map (\p -> Map.findWithDefault Empty p layout)
            [ point { y = y point - 1 }, point { y = y point + 1 }, point { x = x point - 1 }, point { x = x point + 1 }]
    in length $ filter (== Bug) neighbours

recursiveNeighbours :: Int -> Point -> Layers -> [Tile]
recursiveNeighbours level point layers = map (\(l, p) -> Map.findWithDefault Empty p $ Map.findWithDefault (emptyLayer 5) l layers)
        (pointsAbove level point ++ pointsToLeft level point ++ pointsToRight level point ++ pointsBelow level point)

pointsToLeft :: Int -> Point -> [(Int, Point)]
pointsToLeft level Point { x = 0, y = _ } = [(level - 1, Point { x = 1, y = 2 })]
pointsToLeft level Point { x = 3, y = 2 } = [(level + 1, Point { x = 4, y = y }) | y <- [0..4]]
pointsToLeft level point = [(level, point { x = x point - 1 })]

pointsToRight :: Int -> Point -> [(Int, Point)]
pointsToRight level Point { x = 4, y = _ } = [(level - 1, Point { x = 3, y = 2 })]
pointsToRight level Point { x = 1, y = 2 } = [(level + 1, Point { x = 0, y = y }) | y <- [0..4]]
pointsToRight level point = [(level, point { x = x point + 1 })]

pointsAbove :: Int -> Point -> [(Int, Point)]
pointsAbove level Point { x = _, y = 0 } = [(level - 1, Point { x = 2, y = 1 })]
pointsAbove level Point { x = 2, y = 3 } = [(level + 1, Point { x = x, y = 4 }) | x <- [0..4]]
pointsAbove level point = [(level, point { y = y point - 1 })]

pointsBelow :: Int -> Point -> [(Int, Point)]
pointsBelow level Point { x = _, y = 4 } = [(level - 1, Point { x = 2, y = 3 })]
pointsBelow level Point { x = 2, y = 1 } = [(level + 1, Point { x = x, y = 0 }) | x <- [0..4]]
pointsBelow level point = [(level, point { y = y point + 1 })]

nextTile :: Int -> Tile -> Tile
nextTile bugs tile = case (tile, bugs) of
    (Centre, _) -> Centre
    (Empty, 1) -> Bug
    (Empty, 2) -> Bug
    (Bug, 1) -> Bug
    (_, _) -> Empty

countBugs :: Layers -> Int
countBugs = sum . Map.elems . Map.map (length . filter (== Bug) . Map.elems)

biodiversity :: Layout -> Int
biodiversity layout =
    let width = x (maximumBy (comparing x) $ Map.keys layout) + 1
    in sum 
        $ map (biodiversityForTile width)
        $ Map.keys
        $ Map.filter (== Bug) layout

biodiversityForTile :: Int -> Point -> Int
biodiversityForTile width point = 2 ^ ((y point * width) + x point)

emptyLayer :: Int -> Layout
emptyLayer size = 
    let centre = size `div` 2
    in Map.fromList $ do
        row <- [0..size - 1]
        col <- [0..size - 1]
        let tile = if row == centre && col == centre
            then Centre
            else Empty
        pure (Point { x = col, y = row }, tile)

createLayers :: String -> Layers
createLayers input = Map.singleton 0 $ createLayout input

createLayout :: String -> Layout
createLayout input = Map.fromList $ do
    (y, row) <- zip [0 ..] $ lines input
    (x, c  ) <- zip [0 ..] row
    pure (Point { x = x, y = y }, createTile c)

createTile :: Char -> Tile
createTile '#' = Bug
createTile '.' = Empty
createTile '?' = Centre

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s 
                           then Just x
                           else dup' xs (Set.insert x s)