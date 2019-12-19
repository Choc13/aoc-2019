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
type Cave = Map.Map Point Char
type KeyGraph = Map.Map Key [(Key, Int, KeyChain)]
newtype KeyChain = KeyChain String
    deriving (Eq, Ord, Show)
type Key = Char

answer1 :: String -> Maybe Int
answer1 input =
    let cave       = createCave input
        [entrance] = Map.keys $ Map.filter (== '@') cave
        keyGraph   = createKeyGraph cave entrance
        initiallyReachableKeys =
                Map.fromList
                    $ map (\(k, v) -> (KeyChain [cave Map.! k], v))
                    $ findKeys cave (Map.keysSet keyGraph) (Map.singleton entrance 0) [entrance]
    in shortestPath keyGraph
                    initiallyReachableKeys
                    (Map.keysSet initiallyReachableKeys)

answer2 :: String -> Int
answer2 input =
    let cave       = createCave input
        entrances = Map.keys $ Map.filter (== '@') cave
        keyGraph = createKeyGraph cave
        initialKeys graph entrance =
            Map.fromList
                $ map (\(k, v) -> (KeyChain [cave Map.! k], v))
                $ findKeys cave (Map.keysSet graph) (Map.singleton entrance 0) [entrance]
    in sum 
        $ map (\e -> 
            let 
                graph = keyGraph e
                initial = initialKeys graph e 
            in fromMaybe (error "failed") $ shortestPath graph initial (Map.keysSet initial)
        ) entrances

shortestPath
    :: KeyGraph -> Map.Map KeyChain Int -> Set.Set KeyChain -> Maybe Int
shortestPath keyGraph visited frontier = if Set.null frontier
    then Nothing
    else
        let
            keyChain       = Set.findMin frontier
            KeyChain chain = keyChain
            nextFrontier   = Set.delete keyChain frontier
            distance = Map.findWithDefault (error "KeyChain is not in the map") keyChain visited
            nextKeyChains =
                Map.fromList
                    $ filter
                          (\(kc, d) -> case Map.lookup kc visited of
                              Nothing   -> True
                              Just prev -> d < prev
                          )
                    $ neighbourKeys keyGraph keyChain distance
        in
            if length chain == Map.size keyGraph
                then 
                    let otherDistance = shortestPath keyGraph visited nextFrontier
                    in case otherDistance of
                        Nothing -> Just distance
                        Just other -> Just (min distance other)
                else shortestPath
                    keyGraph
                    (Map.union nextKeyChains visited)
                    (Set.union (Map.keysSet nextKeyChains) nextFrontier)

neighbourKeys :: KeyGraph -> KeyChain -> Int -> [(KeyChain, Int)]
neighbourKeys keyGraph (KeyChain chain) distance =
    map (\(k, d, _) -> (KeyChain (k : sort chain), d + distance))
        $ filter
              (\(k, _, KeyChain doors) ->
                let unlockableDoors = filter (\d -> Set.member d $ Map.keysSet keyGraph) doors
                in k `notElem` chain && sort unlockableDoors `isSubsequenceOf` sort chain
              )
        $ Map.findWithDefault (error "Couldn't find head of chain")
                              (head chain)
                              keyGraph

createKeyGraph :: Cave -> Point -> KeyGraph
createKeyGraph cave entrance =
    let keyPoints = allKeys cave (Set.singleton entrance) [entrance]
        accessible = Set.fromList $ map (cave Map.!) keyPoints
    in Map.fromList
        $ map
              (\k ->
                  ( Map.findWithDefault (error "Couldn't find key in cave") k cave
                  , findOtherKeys cave accessible (Map.singleton k 0) [(k, KeyChain "")] k
                  )
              ) keyPoints

findOtherKeys :: Cave
    -> Set.Set Key
    -> Map.Map Point Int
    -> [(Point, KeyChain)]
    -> Point
    -> [(Key, Int, KeyChain)]
findOtherKeys cave accessible visited frontier self =
    let
        canMoveTo p =
            Map.notMember p visited && Map.findWithDefault '#' p cave /= '#'
    in case frontier of
        [] -> []
        ((pos, KeyChain doors):nextFrontier) ->
            let
                tile       = cave Map.! pos
                distance   = visited Map.! pos
                nextPoints = 
                    Map.fromList 
                        $ filter (\(p, _) -> canMoveTo p) 
                        $ neighbours pos distance
                newDoors  = sort ([ toLower tile | isUpper tile && Set.member (toLower tile) accessible ] ++ doors)
                newKeys = [ (tile, distance, KeyChain doors) | isLower tile && pos /= self ]
            in newKeys ++ findOtherKeys cave accessible (Map.union visited nextPoints) (nextFrontier ++ map (, KeyChain newDoors) (Map.keys nextPoints)) self

findKeys :: Cave -> Set.Set Key -> Map.Map Point Int -> [Point] -> [(Point, Int)]
findKeys cave accessible visited frontier =
    let 
        space p = Map.findWithDefault '#' p cave
        canMoveTo p =
            Map.notMember p visited
                && space p /= '#'
                && (not (isUpper (space p)) || Set.notMember (toLower $ space p) accessible)
    in case frontier of
        [] -> []
        (pos : nextFrontier) ->
            let
                tile     = cave Map.! pos
                distance = visited Map.! pos
                nextPoints =
                    Map.fromList
                        $ filter (\(p, _) -> canMoveTo p)
                        $ neighbours pos distance
                newKeys = [ (pos, distance) | isLower tile ]
            in
                newKeys ++ findKeys
                    cave
                    accessible
                    (Map.union visited nextPoints)
                    (nextFrontier ++ Map.keys nextPoints)

allKeys :: Cave -> Set.Set Point -> [Point] -> [Point]
allKeys cave visited frontier =
    let canMoveTo p =
                Set.notMember p visited && Map.findWithDefault '#' p cave /= '#'
    in
        case frontier of
            [] -> []
            (pos : nextFrontier) ->
                let
                    tile = cave Map.! pos
                    nextPoints =
                        Set.fromList $ filter canMoveTo $ map fst $ neighbours
                            pos
                            0
                    newKeys = [ pos | isLower tile ]
                in
                    newKeys ++ allKeys
                        cave
                        (Set.union visited nextPoints)
                        (nextFrontier ++ Set.elems nextPoints)

neighbours :: Point -> Int -> [(Point, Int)]
neighbours pos distance =
    [ (pos { y = y pos - 1 }, distance + 1)
    , (pos { y = y pos + 1 }, distance + 1)
    , (pos { x = x pos - 1 }, distance + 1)
    , (pos { x = x pos + 1 }, distance + 1)
    ]

createCave :: String -> Cave
createCave input = Map.fromList $ do
    (y, row) <- zip [0 ..] $ lines input
    (x, c  ) <- zip [0 ..] row
    pure (Point { x = x, y = y }, c)
