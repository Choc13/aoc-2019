module Lib
    ( module Lib
    )
where

import           Data.List
import qualified Data.Map as Map
import Data.Maybe

type Element = String
type Quantity = Int
data Formula = Formula { quantity :: Quantity, inputs :: Map.Map Element Quantity }
    deriving (Show)

answer1 :: Map.Map Element Formula -> Int
answer1 formulas = fuelOre $ solve formulas

binarySearch :: Int -> Int -> Int -> Map.Map Element Formula -> Int
binarySearch maxOre start end formulas =
    let
        midPoint = start + ((end - start) `div` 2)
        solution = solve $ mapFuel (* midPoint) formulas
        ore = fuelOre solution
    in if midPoint == start
        then midPoint
        else if ore > maxOre
            then binarySearch maxOre start midPoint formulas
            else binarySearch maxOre midPoint end formulas

mapFuel ::  (Int -> Int) -> Map.Map Element Formula -> Map.Map Element Formula
mapFuel f = Map.mapWithKey 
    (\elem formula -> if elem == "FUEL" 
        then formula { quantity = f (quantity formula), inputs = Map.map f (inputs formula) } 
        else formula)

fuelOre :: Map.Map Element Formula -> Int
fuelOre formulas = fromMaybe 0 $ (Map.lookup "ORE" . inputs) =<< Map.lookup "FUEL" formulas

solve :: Map.Map Element Formula -> Map.Map Element Formula
solve formulas = case Map.lookup "FUEL" formulas of
    Nothing -> error "No FUEL formula"
    Just fuelFormula ->
        case
            find (\input -> fst input /= "ORE" && snd input > 0)
            $ Map.assocs
            $ inputs fuelFormula
        of
            Nothing -> formulas
            Just input ->
                let
                    element = fst input
                    elementQuantity = snd input
                    substitution = Map.findWithDefault (error "missing formula") element formulas
                in
                    solve
                    $ Map.insert "FUEL" (substitute fuelFormula element elementQuantity substitution) formulas

substitute :: Formula -> Element -> Quantity -> Formula -> Formula
substitute formula element elementQuantity substitution =
    let 
        multiple = ceiling $ fromIntegral elementQuantity / fromIntegral (quantity substitution)
    in
        formula {
            inputs = Map.unionWith (+) (inputs formula)
                $ Map.map (* multiple)
                $ Map.insert element (-1 * quantity substitution)
                $ inputs substitution
        }