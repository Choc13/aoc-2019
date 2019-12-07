module Lib
( module Lib
) where
    
import Data.Array
import Data.List
import qualified Data.Map as Map

answer :: [String] -> Int
answer input = totalOrbits 0 "COM" $ buildMap input

answer2:: [String] -> (String, Int)
answer2 input = closestAncestor (buildMap input) "YOU" "SAN"

totalOrbits :: Int -> String -> Map.Map String [String] -> Int
totalOrbits count com orbits = count + case Map.lookup com orbits of
    Nothing -> 0
    Just orbitees -> foldl (\acc x -> acc + totalOrbits (count + 1) x orbits) 0 orbitees

closestAncestor :: Map.Map String [String] -> String -> String -> (String, Int)
closestAncestor orbits a b = head $ sortOn snd $ Map.toList $ Map.intersectionWith (+) (Map.fromList $ pathTo orbits a "COM") (Map.fromList $ pathTo orbits b "COM")

pathTo :: Map.Map String [String] -> String -> String -> [(String, Int)]
pathTo orbits to from
    | from == to = [(from, 0)]
    | otherwise = case Map.lookup from orbits of
        Nothing -> []
        Just orbitees ->
            let childPath = concatMap (pathTo orbits to) orbitees
            in if null childPath then [] else (from, 1 + snd (head childPath)) : childPath

buildMap :: [String] -> Map.Map String [String]
buildMap input = Map.fromListWith (++) $ map createOrbitalPair input

createOrbitalPair :: String -> (String, [String])
createOrbitalPair (c:o:m:')':orbiter) = ([c,o,m], [orbiter])
createOrbitalPair (c:o:')':orbiter) = ([c,o], [orbiter])
createOrbitalPair (c:')':orbiter) = ([c], [orbiter])
