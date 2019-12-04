module Lib
    ( module Lib
    ) where

import Data.Array

findInputs :: Int -> Int -> [Int] -> Int
findInputs noun verb program
    | output == 19690720 = 100 * noun + verb
    | noun < 100 = findInputs (noun + 1) verb program
    | verb < 100 = findInputs 0 (verb + 1) program
    | otherwise = -1
    where
        output:xs = run noun verb program

run :: Int -> Int -> [Int] -> [Int]
run noun verb program = 
    let 
        input = listArray (0, length program - 1) program // [(1, noun)] // [(2, verb)]
    in
        elems $ runRecursive 0 input

runRecursive :: Int -> Array Int Int -> Array Int Int
runRecursive ix a
    | code == 1 = runRecursive (ix + 4) (runInstruction (+))
    | code == 2 = runRecursive (ix + 4) (runInstruction (*))
    | code == 99 = a
    where
        code = a ! ix
        runInstruction op = 
            let
                param x = a ! (a ! x)
                val = op (param (ix + 1)) (param (ix + 2))
            in a // [(a ! (ix + 3), val)]
