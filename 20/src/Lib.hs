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
data Space = Empty | Portal String
    deriving (Eq, Ord, Show)
type Maze = Map.Map Point Space

answer1 :: String -> Maybe Int
answer1 input = 
    let cave = createMaze input
        [entrance] = Map.keys $ Map.filter (== Portal "AA") cave
    in shortestPath cave (Map.singleton entrance 0) [entrance]

answer2 :: String -> Int
answer2  input = 
    let cave = createMaze input
        [entrance] = Map.keys $ Map.filter (== Portal "AA") cave
    in shortestPath cave (Map.singleton entrance 0) [entrance]

shortestPathWithLevels :: Maze -> Map.Map Point Int -> [Point] -> Maybe Int
shortestPathWithLevels maze visited frontier = case frontier of
    [] -> Nothing
    (pos : nextFrontier) ->
        let space    = maze Map.! pos
            distance = visited Map.! pos
            adjacent =
                    Map.fromList
                        $ map (, distance + 1)
                        $ filter (`Map.notMember` visited)
                        $ neighbours maze pos
        in  case space of
                Portal "ZZ" -> Just distance
                _           -> shortestPath maze
                                            (Map.union visited adjacent)
                                            (nextFrontier ++ Map.keys adjacent)

shortestPath :: Maze -> Map.Map Point Int -> [Point] -> Maybe Int
shortestPath maze visited frontier = case frontier of
    [] -> Nothing
    (pos : nextFrontier) ->
        let space    = maze Map.! pos
            distance = visited Map.! pos
            adjacent =
                    Map.fromList
                        $ map (, distance + 1)
                        $ filter (`Map.notMember` visited)
                        $ neighbours maze pos
        in  case space of
                Portal "ZZ" -> Just distance
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
    Portal portal -> listToMaybe $ Map.keys $ Map.filterWithKey (\k v -> k /= entrance && v == Portal portal) maze
    Empty -> Nothing

createMaze :: String -> Maze
createMaze input =
    let charMap = createCharMap input
    in  Map.map (\(Just space) -> space) $ Map.filter isJust $ Map.mapWithKey
            (createSpace charMap)
            charMap

createSpace :: Map.Map Point Char -> Point -> Char -> Maybe Space
createSpace charMap pos c
    | c == '.'
    = let above   = [pos, pos { y = y pos - 2 }, pos { y = y pos - 1 }]
          below   = [pos, pos { y = y pos + 1 }, pos { y = y pos + 2 }]
          left    = [pos, pos { x = x pos - 2 }, pos { x = x pos - 1 }]
          right   = [pos, pos { x = x pos + 1 }, pos { x = x pos + 2 }]
          portals = filter (\[_, a, b] -> isUpper a && isUpper b) 
                $ map (map (\p -> Map.findWithDefault ' ' p charMap))
                [above, below, left, right]
      in  case portals of
            [['.', a, b]] -> Just (Portal [a, b])
            []            -> Just Empty
    | otherwise
    = Nothing

createPortal :: String -> Maybe Space
createPortal input =
    let portal a b =
                if isUpper a && isUpper b then Just (Portal [a, b]) else Nothing
    in  case input of
            ['.', a, b  ] -> portal a b
            [a  , b, '.'] -> portal a b
            _             -> Nothing

createCharMap :: String -> Map.Map Point Char
createCharMap input = Map.fromList $ do
    (y, row) <- zip [0 ..] $ lines input
    (x, c  ) <- zip [0 ..] row
    pure (Point { x = x, y = y }, c)
