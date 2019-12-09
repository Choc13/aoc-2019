{-# LANGUAGE RecordWildCards #-}

module Lib
    ( module Lib
    )
where

import           Data.Maybe
import qualified Data.Map                      as Map

type Program = Map.Map Int Int
type InstructionPointer = Int
type RelativeBase = Int
type Param = Int
type Mode = Int
data OpCode = Add | Multiply | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equal | RelativeBaseOffset | Halt
    deriving (Show, Eq)
data Instruction = Instruction { opCode :: OpCode, paramModes :: [Mode] }
    deriving (Show, Eq)
data ProgramState = ProgramState { program :: Program, ip :: InstructionPointer, rb :: RelativeBase }
    deriving (Show, Eq)

boost :: ProgramState -> Int -> IO ()
boost state input = case run state (Just input) of
    Left  _                  -> print "Program halted"
    Right (newState, output) -> case output of
        Nothing -> print "More input required"
        Just o  -> do
            print o
            boost newState input

run :: ProgramState -> Maybe Int -> Either () (ProgramState, Maybe Int)
run ProgramState {..} input = case opCode of
    Add ->
        run ProgramState { program = binaryOp (+), ip = ip + 4, rb = rb } input
    Multiply ->
        run ProgramState { program = binaryOp (*), ip = ip + 4, rb = rb } input
    Input -> case input of
        Just i -> run
            ProgramState { program = updateProgram 1 i, ip = ip + 2, rb = rb }
            Nothing
        Nothing -> Right
            (ProgramState { program = program, ip = ip, rb = rb }, Nothing)
    Output -> Right
        ( ProgramState { program = program, ip = ip + 2, rb = rb }
        , Just $ param 1
        )
    JumpIfTrue -> run
        ProgramState { program = program, ip = jumpIf (/= 0), rb = rb }
        input
    JumpIfFalse -> run
        ProgramState { program = program, ip = jumpIf (== 0), rb = rb }
        input
    LessThan -> run
        ProgramState { program = binaryOp (\p1 p2 -> if p1 < p2 then 1 else 0)
                     , ip      = ip + 4
                     , rb      = rb
                     }
        input
    Equal -> run
        ProgramState { program = binaryOp (\p1 p2 -> if p1 == p2 then 1 else 0)
                     , ip      = ip + 4
                     , rb      = rb
                     }
        input
    RelativeBaseOffset -> run
        ProgramState { program = program, ip = ip + 2, rb = rb + param 1 }
        input
    Halt -> Left ()
  where
    Instruction {..} = (parseInstruction . read) ip
    read ip = fromMaybe 0 $ Map.lookup ip program
    paramAddr ix = case paramModes !! (ix - 1) of
        0 -> read (ip + ix)
        1 -> ip + ix
        2 -> rb + read (ip + ix)
    param = read . paramAddr
    updateProgram ix val = Map.insert (paramAddr ix) val program
    binaryOp op = updateProgram 3 $ op (param 1) (param 2)
    jumpIf pred = if pred (param 1) then param 2 else ip + 3

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
    9  -> RelativeBaseOffset
    99 -> Halt

parseParamMode :: Int -> Int -> Mode
parseParamMode i ix = (i `div` (10 ^ (ix + 1))) `mod` 10
