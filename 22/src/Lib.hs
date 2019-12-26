module Lib
    ( module Lib
    )
where

import           Data.Char
import           Data.List
import           Data.Maybe

data Shuffle = Cut Integer | Reverse | DealIncrement Integer
    deriving (Eq, Ord, Show)
data Coeffs = Coeffs { a :: Integer, b :: Integer }
    deriving (Eq, Ord, Show)
type DeckSize = Integer
type Position = Integer

answer1 :: [String] -> Position
answer1 input = shuffle 10007 2019 $ parseInstructions input

answer2 :: [String] -> Coeffs
answer2 input = 
    let shuffles = parseInstructions input
        invShuffles = reverse shuffles
    in invShuffleCoeffs 119315717514047 2020 invShuffles

combineShuffles :: DeckSize -> [Shuffle] -> Coeffs
combineShuffles deckSize = foldl (\coeffs shuff -> case shuff of
    Reverse -> coeffs { a = (-a coeffs) `mod` deckSize, b = (-b coeffs - 1) `mod` deckSize}
    Cut n -> coeffs { b = (b coeffs - n) `mod` deckSize }
    DealIncrement n -> coeffs { a = (a coeffs * n) `mod` deckSize, b = (b coeffs * n) `mod` deckSize }) Coeffs { a = 1, b = 0 }

applyCoeffs :: DeckSize -> Integer -> Coeffs -> Integer
applyCoeffs deckSize x coeffs = (((a coeffs * x) `mod` deckSize) + b coeffs) `mod` deckSize

rapidCombine :: Integer -> DeckSize -> Coeffs -> Coeffs
rapidCombine 1 deckSize coeffs = coeffs 
rapidCombine n deckSize coeffs
    | n `mod` 2 == 0 = 
        let halfCoeffs = rapidCombine (n `div` 2) deckSize coeffs 
        in combineCoeffs deckSize halfCoeffs halfCoeffs
    | otherwise = combineCoeffs deckSize coeffs (rapidCombine (n - 1) deckSize coeffs)

combineCoeffs :: DeckSize -> Coeffs -> Coeffs -> Coeffs
combineCoeffs deckSize first second = Coeffs { 
    a = (a second * a first) `mod` deckSize,
    b = ((a second * b first) + b second) `mod` deckSize }

manyInvShuffles :: DeckSize -> Position -> [Shuffle] -> [Integer]
manyInvShuffles deckSize pos shuffles = 
    unfoldr (\seed -> Just (seed, invShuffle deckSize seed $ reverse shuffles)) pos

manyShuffles :: DeckSize -> Position -> [Shuffle] -> [Integer]
manyShuffles deckSize pos shuffles = 
    unfoldr (\seed -> Just (seed, shuffle deckSize seed shuffles)) pos

invShuffleCoeffs :: DeckSize -> Position -> [Shuffle] -> Coeffs
invShuffleCoeffs deckSize pos shuffles =
    let firstUnshuffle = invShuffle deckSize pos shuffles
    in Coeffs { a = firstUnshuffle `div` pos, b = firstUnshuffle `mod` pos }

invShuffle :: DeckSize -> Position -> [Shuffle] -> Position
invShuffle deckSize = foldl (doInvShuffle deckSize)

shuffle :: DeckSize -> Position -> [Shuffle] -> Position
shuffle deckSize = foldl (doShuffle deckSize)

doShuffle :: DeckSize -> Position -> Shuffle -> Position
doShuffle deckSize position shuffle = case shuffle of
    Reverse -> (deckSize - 1) - position
    Cut n -> (position - n) `mod` deckSize
    DealIncrement n -> (position * n) `mod` deckSize

doInvShuffle :: DeckSize -> Position -> Shuffle -> Position
doInvShuffle deckSize position shuffle = case shuffle of
    Reverse -> (deckSize - 1) - position
    Cut n -> (position + n) `mod` deckSize
    DealIncrement n -> (position * fromMaybe (error "No modInv") (modInv n deckSize)) `mod` deckSize

parseInstructions :: [String] -> [Shuffle]
parseInstructions = map parseInstruction

parseInstruction :: String -> Shuffle
parseInstruction i = case words i of
    ["deal", "into", "new", "stack"] -> Reverse
    ["cut", x]                       -> Cut $ read x
    ["deal", "with", "increment", x] -> DealIncrement $ read x

-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Integer -> Integer -> Maybe Integer
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x
 
-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)
