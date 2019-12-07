{-# LANGUAGE RecordWildCards #-}

module Lib
    ( module Lib
    )
where

import           Data.Array
import qualified Data.Map as Map

type Program = Array Int Int
type InstructionPointer = Int
type Param = Int
type Mode = Int
data OpCode = Add | Multiply | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equal | Halt
    deriving (Show, Eq)
data Instruction = Instruction { opCode :: OpCode, paramModes :: [Mode] }
    deriving (Show, Eq)

answer :: Program -> Int
answer program = maximum $ map (runAmps program) (generatePhases 5 9)

runAmps :: Program -> [Int] -> Int 
runAmps program phases = runInitialisedAmps (initAmps program phases) 0 0

initAmps :: Program -> [Int] -> Map.Map Int (Program, InstructionPointer)
initAmps program phases = Map.fromList $ zip [0..4] $ map (initAmp program) phases

initAmp :: Program -> Int -> (Program, InstructionPointer)
initAmp program phase = case run program 0 (Just phase) of
    Left _ -> error "Initialisation failed, program halted"
    Right ((newProg, newIp), output) -> case output of
        Nothing -> (newProg, newIp)
        Just _ -> error "Initialisation failed, program produced output"

runInitialisedAmps :: Map.Map Int (Program, InstructionPointer) -> Int -> Int -> Int
runInitialisedAmps ampMap ampIndex input =
    let
        state = case Map.lookup ampIndex ampMap of
            Just x -> x
            Nothing -> error "Invalid amp index"
        program = fst state
        ip = snd state 
    in case run program ip (Just input) of
        Left answer -> answer
        Right ((newProg, newIp), output) -> case output of 
            Nothing -> error "Program should already be initialised"
            Just o -> runInitialisedAmps (Map.insert ampIndex (newProg, newIp) ampMap) ((ampIndex + 1) `mod` 5) o

run :: Program -> InstructionPointer -> Maybe Int -> Either Int ((Program, InstructionPointer), Maybe Int)
run program ip input
    | opCode == Add = run (binaryOp (+)) (ip + 4) input
    | opCode == Multiply = run (binaryOp (*)) (ip + 4) input
    | opCode == Input = case input of
        Just i -> run (updateProgram (ip + 1) i) (ip + 2) Nothing
        Nothing -> Right ((program, ip), Nothing)
    | opCode == Output = Right ((program, ip + 2), Just $ param 1)
    | opCode == JumpIfTrue = run program (jumpIf (/= 0)) input
    | opCode == JumpIfFalse = run program (jumpIf (== 0)) input
    | opCode == LessThan = run
        (binaryOp (\p1 p2 -> if p1 < p2 then 1 else 0))
        (ip + 4)
        input
    | opCode == Equal = run 
        (binaryOp (\p1 p2 -> if p1 == p2 then 1 else 0))
        (ip + 4)
        input
    | opCode == Halt = case input of
        Nothing -> error "Halted without input"
        Just i -> Left i
  where
    Instruction {..} = parseInstruction $ program ! ip
    param ix | paramModes !! (ix - 1) == 0 = program ! (program ! (ip + ix))
             | paramModes !! (ix - 1) == 1 = program ! (ip + ix)
    updateProgram ip val = program // [(program ! ip, val)]
    binaryOp op = updateProgram (ip + 3) $ op (param 1) (param 2)
    jumpIf pred = if pred (param 1) then param 2 else ip + 3

generatePhases :: Int -> Int -> [[Int]]
generatePhases start end =
    [ [a, b, c, d, e]
    | a <- [start .. end]
    , b <- filter (/= a) [start .. end]
    , c <- filter (\x -> x /= a && x /= b) [start .. end]
    , d <- filter (\x -> x /= a && x /= b && x /= c) [start .. end]
    , e <- filter (\x -> x /= a && x /= b && x /= c && x /= d) [start .. end]
    ]

parseInstruction :: Int -> Instruction
parseInstruction i = Instruction
    { opCode     = parseOpCode i
    , paramModes = map (parseParamMode i) [1, 2, 3]
    }

parseOpCode :: Int -> OpCode
parseOpCode i = case i `mod` 100 of
    1  -> Add
    2  -> Multiply
    3  -> Input
    4  -> Output
    5  -> JumpIfTrue
    6  -> JumpIfFalse
    7  -> LessThan
    8  -> Equal
    99 -> Halt

parseParamMode :: Int -> Int -> Mode
parseParamMode i ix = (i `div` (10 ^ (ix + 1))) `mod` 10
