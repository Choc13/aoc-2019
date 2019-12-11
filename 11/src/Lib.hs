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
data Point = Point { x :: Int, y :: Int }
    deriving (Show, Eq, Ord)
data Heading = Up | Down | Left | Right
type Drawing = Map.Map Point Int

answer1 :: Program -> Int
answer1 program = Map.size $ 
    runRobot ProgramState { program = program, ip = 0, rb = 0 } Point { x = 0, y = 0 } Up $ 
    Map.fromList []

answer2 :: Program -> Drawing
answer2 program =
    runRobot ProgramState { program = program, ip = 0, rb = 0 } Point { x = 0, y = 0 } Up $ 
    Map.fromList [(Point { x = 0, y = 0 }, 1)]

runRobot :: ProgramState -> Point -> Heading -> Drawing -> Drawing
runRobot state pos heading drawing =
    let
        input = fromMaybe 0 $ Map.lookup pos drawing
    in case run state (Just input) of
        Prelude.Left _ -> drawing
        Prelude.Right (newState1, output1) -> case output1 of
            Nothing -> error "More input required"
            Just color -> case run newState1 (Just input) of
                Prelude.Left _ -> error "Program halted before outputting direction"
                Prelude.Right (newState2, output2) -> case output2 of 
                    Nothing -> error "More input required"
                    Just direction -> 
                        let 
                            newHeading = turn heading direction
                            newPos = moveForward newHeading pos
                        in runRobot newState2 newPos newHeading $ Map.insert pos color drawing

turn :: Heading -> Int -> Heading
turn Up 0 = Lib.Left
turn Lib.Left 0 = Down
turn Down 0 = Lib.Right
turn Lib.Right 0 = Up
turn Up 1 = Lib.Right
turn Lib.Right 1 = Down
turn Down 1 = Lib.Left
turn Lib.Left 1 = Up

moveForward :: Heading -> Point -> Point
moveForward Up pos = pos { y = y pos - 1 }
moveForward Down pos = pos { y = y pos + 1 }
moveForward Lib.Left pos = pos { x = x pos - 1 }
moveForward Lib.Right pos = pos { x = x pos + 1 }

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
        Nothing -> Prelude.Right
            (ProgramState { program = program, ip = ip, rb = rb }, Nothing)
    Output -> Prelude.Right
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
    Halt -> Prelude.Left ()
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
