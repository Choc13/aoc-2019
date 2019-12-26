module Main where

import           Lib
import Data.List
import Data.Maybe

main :: IO ()
main = do
    f <- readFile "input.txt"
    let shuffles = parseInstructions $ lines f
    let deckSize = 119315717514047
    let position = 2020

    let series = take 3 $ manyShuffles deckSize position shuffles
    print series

    let coeffs = combineShuffles deckSize shuffles
    print coeffs

    print "Trying first in series"
    print $ applyCoeffs deckSize (head series) coeffs
    print $ applyCoeffs deckSize (series !! 1) coeffs
    print "Should be the same as above"
    print $ applyCoeffs deckSize (head series) (combineCoeffs deckSize coeffs coeffs)

    putStrLn ""
    print "Inverting..."
    let invSeries = take 3 $ manyInvShuffles deckSize position shuffles
    print invSeries
    
    let mmi = fromMaybe (error "Boom") (modInv (a coeffs) deckSize)
    let invCoeffs = Coeffs { a = mmi `mod` deckSize, b = (-b coeffs * mmi) `mod` deckSize}
    print invCoeffs
    
    print "Testing inverse"
    print $ applyCoeffs deckSize (head invSeries) invCoeffs
    print $ applyCoeffs deckSize (invSeries !! 1) invCoeffs
    print "Should be the same as above"
    print $ applyCoeffs deckSize (head invSeries) (combineCoeffs deckSize invCoeffs invCoeffs)

    print "Answer"
    let finalCoeffs = rapidCombine 101741582076661 deckSize invCoeffs
    print finalCoeffs
    print $ applyCoeffs deckSize position finalCoeffs
