{-# LANGUAGE RecordWildCards #-}

module Lib
( module Lib
) where
    
import Data.Array
    
type Mode = Int

data Instruction = Instruction { opCode :: Int, paramModes :: [Mode] }
        deriving (Show, Eq)

run :: Array Int Int -> Int -> IO ()
run program ip
    | opCode == 1 = run (binaryOp (+)) (ip + 4)
    | opCode == 2 = run (binaryOp (*)) (ip + 4)
    | opCode == 3 = do
        print "Input a value:"
        input <- getLine
        run (updateProgram (ip + 1) $ read input) (ip + 2)
    | opCode == 4 = do 
        print $ param 1
        run program (ip + 2)
    | opCode == 5 = run program $ jumpIf (/= 0)
    | opCode == 6 = run program $ jumpIf (== 0)
    | opCode == 7 = run (binaryOp (\p1 p2 -> if p1 < p2 then 1 else 0)) (ip + 4)
    | opCode == 8 = run (binaryOp (\p1 p2 -> if p1 == p2 then 1 else 0)) (ip + 4)
    | opCode == 99 = return ()
    where
        Instruction {..} = parseInstruction $ program ! ip
        param ix
            | paramModes !! (ix - 1) == 0 = program ! (program ! (ip + ix))
            | paramModes !! (ix - 1) == 1 = program ! (ip + ix)
        updateProgram ip val = program // [(program ! ip, val)]
        binaryOp op = updateProgram (ip + 3) $ op (param 1) (param 2)
        jumpIf pred = if pred (param 1) then param 2 else ip + 3

param :: Int -> Mode -> Array Int Int -> Int
param ip mode program
    | mode == 0 = program ! (program ! ip)
    | mode == 1 = program ! ip

parseInstruction :: Int -> Instruction
parseInstruction i =
    Instruction {
        opCode = parseOpCode i,
        paramModes = map (parseParamMode i) [1, 2, 3]
    }

parseOpCode :: Int -> Int
parseOpCode i = i `mod` 100

parseParamMode :: Int -> Int -> Int
parseParamMode i ix = (i `div` (10 ^ (ix + 1))) `mod` 10
