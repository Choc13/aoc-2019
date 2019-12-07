{-# LANGUAGE RecordWildCards #-}

module Lib
    ( module Lib
    )
where

import           Data.Array

type Program = Array Int Int
type InstructionPointer = Int
type Param = Int
type Mode = Int
data OpCode = Add | Multiply | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equal | Halt
    deriving (Show, Eq)
data Instruction = Instruction { opCode :: OpCode, paramModes :: [Mode] }
    deriving (Show, Eq)

run :: Program -> InstructionPointer -> IO ()
run program ip
    | opCode == Add = run (binaryOp (+)) (ip + 4)
    | opCode == Multiply = run (binaryOp (*)) (ip + 4)
    | opCode == Input = do
        print "Input a value:"
        input <- getLine
        run (updateProgram (ip + 1) $ read input) (ip + 2)
    | opCode == Output = do
        print $ param 1
        run program (ip + 2)
    | opCode == JumpIfTrue = run program $ jumpIf (/= 0)
    | opCode == JumpIfFalse = run program $ jumpIf (== 0)
    | opCode == LessThan = run
        (binaryOp (\p1 p2 -> if p1 < p2 then 1 else 0))
        (ip + 4)
    | opCode == Equal = run (binaryOp (\p1 p2 -> if p1 == p2 then 1 else 0))
                            (ip + 4)
    | opCode == Halt = return ()
  where
    Instruction {..} = parseInstruction $ program ! ip
    param ix | paramModes !! (ix - 1) == 0 = program ! (program ! (ip + ix))
             | paramModes !! (ix - 1) == 1 = program ! (ip + ix)
    updateProgram ip val = program // [(program ! ip, val)]
    binaryOp op = updateProgram (ip + 3) $ op (param 1) (param 2)
    jumpIf pred = if pred (param 1) then param 2 else ip + 3

param :: Program -> InstructionPointer -> Int -> Instruction -> Param
param program ip ix Instruction { paramModes = modes } =
    let mode      = modes !! (ix - 1)
        immediate = program ! (ip + ix)
    in  if mode == 0 then program ! immediate else immediate

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
