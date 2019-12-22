{-# LANGUAGE TupleSections #-}

module Lib
    ( module Lib
    )
where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Ord                       ( comparing )

data Point = Point { x :: Int, y :: Int }
    deriving (Show, Eq, Ord)
data DonutSide = Outer | Inner
    deriving (Eq, Ord, Show)
data Space = Empty | Portal String DonutSide
    deriving (Eq, Ord, Show)
type Maze = Map.Map Point Space
type Level = Int
type Distance = Int

answer1 :: String -> Maybe Int
answer1 input = 
    let maze = createMaze input
        [entrance] = Map.keys $ Map.filter (== Portal "AA" Outer) maze
    in shortestPath maze (Map.singleton entrance 0) [entrance]

answer2 :: String -> Maybe Int
answer2  input = 
    let maze = createMaze input
        entryPortal = Portal "AA" Outer
        [entrance] = Map.keys $ Map.filter (== entryPortal) maze
        initialState = (entrance, entryPortal, 0)
    in shortestPathWithLevels maze (Map.singleton (entrance, entryPortal, 0) 0) [initialState]

shortestPathWithLevels :: Maze -> Map.Map (Point, Space, Level) Distance -> [(Point, Space, Level)] -> Maybe Int
shortestPathWithLevels maze visited frontier = case frontier of
    [] -> Nothing
    ((pos, portal, level) : nextFrontier) ->
        let space    = maze Map.! pos
            distance = visited Map.! (pos, portal, level)
            adjacent =
                    Map.fromList
                        $ map (, distance + 1)
                        $ filter (`Map.notMember` visited)
                        $ neighboursWithPortals maze pos portal level
        in  case (space, level) of
            (Portal "ZZ" Outer, 0) -> Just distance
            _           -> shortestPathWithLevels
                            maze
                            (Map.union visited adjacent)
                            (nextFrontier ++ Map.keys adjacent)

level :: [Space] -> Int
level portals = sum $ map (\(Portal p s) -> if s == Inner then 1 else - 1) portals

neighboursWithPortals :: Maze -> Point -> Space -> Level -> [(Point, Space, Level)]
neighboursWithPortals maze pos portal level = 
    maybeToList (findPortalExit2 maze pos level) ++
        map (, portal, level)
            (filter (`Map.member` maze)
                [ (pos { y = y pos - 1 })
                , (pos { y = y pos + 1 })
                , (pos { x = x pos - 1 })
                , (pos { x = x pos + 1 })
                ])

-- Making the assumption that ZZ portals not at level 0 just get filtered out here and so are effectively walls
findPortalExit2 :: Maze -> Point -> Level -> Maybe (Point, Space, Level)
findPortalExit2 maze entrance prevLevel = case maze Map.! entrance of
    Portal p s -> listToMaybe
        $ filter (\(_, _, l) -> l >= 0)
        $ map (\(p, exit) -> (p, exit, prevLevel + if s == Inner then 1 else -1))
        $ Map.assocs
        $ Map.filter (isOppositePortal (Portal p s)) maze
    Empty -> Nothing

shortestPath :: Maze -> Map.Map Point Int -> [Point] -> Maybe Int
shortestPath maze visited frontier = case frontier of
    [] -> Nothing
    (pos : nextFrontier) ->
        let space    = maze Map.! pos
            distance = visited Map.! pos
            adjacent = Map.fromList
                $ map (, distance + 1)
                $ filter (`Map.notMember` visited)
                $ neighbours maze pos
        in  case space of
                Portal "ZZ" Outer -> Just distance
                _           -> shortestPath maze
                                            (Map.union visited adjacent)
                                            (nextFrontier ++ Map.keys adjacent)

neighbours :: Maze -> Point -> [Point]
neighbours maze pos = filter
    (`Map.member` maze)
    [ pos { y = y pos - 1 }
    , pos { y = y pos + 1 }
    , pos { x = x pos - 1 }
    , pos { x = x pos + 1 }
    ] ++ maybeToList (findPortalExit maze pos)

findPortalExit :: Maze -> Point -> Maybe Point
findPortalExit maze entrance = case maze Map.! entrance of
    Portal p s -> listToMaybe 
        $ Map.keys 
        $ Map.filter (isOppositePortal (Portal p s)) maze
    Empty -> Nothing

isOppositePortal :: Space -> Space -> Bool
isOppositePortal _ Empty = False
isOppositePortal (Portal p1 s1) (Portal p2 s2) = s1 /= s2 && p1 == p2

createMaze :: String -> Maze
createMaze input =
    let charMap = createCharMap input
        points = Map.keysSet charMap
        maxY   = y $ maximumBy (comparing y) points
        maxX   = x $ maximumBy (comparing x) points
    in  Map.map (\(Just space) -> space) 
            $ Map.filter isJust 
            $ Map.mapWithKey (createSpace charMap maxX maxY) charMap

createSpace :: Map.Map Point Char -> Int -> Int -> Point -> Char -> Maybe Space
createSpace charMap maxX maxY pos c
    | c == '.'
    = let above   = [pos, pos { y = y pos - 2 }, pos { y = y pos - 1 }]
          below   = [pos, pos { y = y pos + 1 }, pos { y = y pos + 2 }]
          left    = [pos, pos { x = x pos - 2 }, pos { x = x pos - 1 }]
          right   = [pos, pos { x = x pos + 1 }, pos { x = x pos + 2 }]
          portals = filter (\[_, a, b] -> isUpper a && isUpper b)
                $ map (map (\p -> Map.findWithDefault ' ' p charMap))
                [above, below, left, right]
      in  case portals of
            [['.', a, b]] -> Just (Portal [a, b] $ donutSide maxX maxY pos )
            []            -> Just Empty
    | otherwise
    = Nothing

donutSide :: Int -> Int -> Point -> DonutSide
donutSide maxX maxY pos = if x pos < 3 || x pos > maxX - 3 || y pos < 3 || y pos > maxY - 3
    then Outer
    else Inner 

createCharMap :: String -> Map.Map Point Char
createCharMap input = Map.fromList $ do
    (y, row) <- zip [0 ..] $ lines input
    (x, c  ) <- zip [0 ..] row
    pure (Point { x = x, y = y }, c)
