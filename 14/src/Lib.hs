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

solve :: Map.Map Element Formula -> Maybe Int
solve formulas = case Map.lookup "FUEL" formulas of
    Nothing -> Nothing
    Just fuelFormula ->
        case
            find (\input -> fst input /= "ORE" && snd input > 0)
            $ Map.assocs
            $ inputs fuelFormula
        of
            Nothing -> Map.lookup "ORE" $ inputs fuelFormula
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