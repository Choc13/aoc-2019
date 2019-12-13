module Lib
    ( module Lib
    )
where

import           Data.List

data Moon = Moon { pos :: [Int], vel :: [Int] }
    deriving (Show, Eq)

data Energy = Energy { kinetic :: Int, potential :: Int }
    deriving (Show, Eq)

phase :: Int -> [Moon] -> Maybe Int
phase dim moons =
    case
        findIndex (\ms -> map (moonPlane dim) ms == map (moonPlane dim) moons)
            $ (tail . simulate) moons
    of
        Nothing -> Nothing
        Just x  -> Just (x + 1)

moonPlane :: Int -> Moon -> (Int, Int)
moonPlane dim moon = (pos moon !! dim, vel moon !! dim)

energyAfter :: Int -> [Moon] -> Int
energyAfter steps = sum . map totalEnergy . last . take (steps + 1) . energies

energies :: [Moon] -> [[Energy]]
energies = map (map energy) . simulate

totalEnergy :: Energy -> Int
totalEnergy energy = potential energy * kinetic energy

energy :: Moon -> Energy
energy moon = Energy { potential = (sum . map abs . pos) moon
                     , kinetic   = (sum . map abs . vel) moon
                     }

simulate :: [Moon] -> [[Moon]]
simulate = unfoldr (\moons -> Just (moons, simulateStep moons))

simulateStep :: [Moon] -> [Moon]
simulateStep moons = map (`updateMoon` moons) moons

updateMoon :: Moon -> [Moon] -> Moon
updateMoon moon moons =
    let newV = zipWith (+) (vel moon) (gravity moon moons)
    in  moon { vel = newV, pos = zipWith (+) (pos moon) newV }

gravity :: Moon -> [Moon] -> [Int]
gravity moon =
    foldl (\acc other -> zipWith (+) (gravityBetween moon other) acc) [0, 0, 0]

gravityBetween :: Moon -> Moon -> [Int]
gravityBetween moonA moonB =
    zipWith (\a b -> signum (b - a)) (pos moonA) (pos moonB)
