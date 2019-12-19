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

-- instance Eq KeyChain where
--     KeyChain a == KeyChain b =
--         head a == head b && sort (tail a) == sort (tail b)

-- instance Ord KeyChain where
--     compare (KeyChain a) (KeyChain b) =
--         case compare (head a) (head b) of
--             EQ -> compare (sort (tail a)) (sort (tail b))
--             LT -> LT
--             GT -> GT 

answer1 :: String -> [(KeyChain, Int)]
answer1 input =
    let cave       = createCave input
        [entrance] = Map.keys $ Map.filter (== '@') cave
        keyGraph   = createKeyGraph cave entrance
        initiallyReachableKeys =
                Map.fromList
                    $ map (\(k, v) -> (KeyChain [cave Map.! k], v))
                    $ findKeys cave (Map.singleton entrance 0) [entrance]
    in  shortestPath keyGraph
                     initiallyReachableKeys
                     (Map.keysSet initiallyReachableKeys)

answer2 :: ()
answer2 = ()

shortestPath
    :: KeyGraph -> Map.Map KeyChain Int -> Set.Set KeyChain -> [(KeyChain, Int)]
shortestPath keyGraph visited frontier = if Set.null frontier
    then []
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
                then (keyChain, distance) : shortestPath keyGraph visited nextFrontier
                else shortestPath
                    keyGraph
                    (Map.union nextKeyChains visited)
                    (Set.union (Map.keysSet nextKeyChains) nextFrontier)

neighbourKeys :: KeyGraph -> KeyChain -> Int -> [(KeyChain, Int)]
neighbourKeys keyGraph (KeyChain chain) distance =
    map (\(k, d, _) -> (KeyChain (k : sort chain), d + distance))
        $ filter
              (\(k, _, KeyChain doors) ->
                  k `notElem` chain && sort doors `isSubsequenceOf` sort chain
              )
        $ Map.findWithDefault (error "Couldn't find head of chain")
                              (head chain)
                              keyGraph

createKeyGraph :: Cave -> Point -> KeyGraph
createKeyGraph cave entrance =
    Map.fromList
        $ map
              (\k ->
                  ( Map.findWithDefault (error "Couldn't find key in cave") k cave
                  , findOtherKeys cave (Map.singleton k 0) [(k, KeyChain "")] k
                  )
              )
        $ allKeys cave (Set.singleton entrance) [entrance]

findOtherKeys :: Cave
    -> Map.Map Point Int
    -> [(Point, KeyChain)]
    -> Point
    -> [(Key, Int, KeyChain)]
findOtherKeys cave visited frontier self =
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
                newDoors  = sort ([ toLower tile | isUpper tile ] ++ doors)
                newKeys = [ (tile, distance, KeyChain doors) | isLower tile && pos /= self ]
            in newKeys ++ findOtherKeys cave (Map.union visited nextPoints) (nextFrontier ++ map (, KeyChain newDoors) (Map.keys nextPoints)) self

findKeys :: Cave -> Map.Map Point Int -> [Point] -> [(Point, Int)]
findKeys cave visited frontier =
    let canMoveTo p =
                Map.notMember p visited
                    && Map.findWithDefault '#' p cave
                    /= '#'
                    && not (isUpper (Map.findWithDefault '#' p cave))
    in
        case frontier of
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
