module Lib
    ( module Lib
    )
where

import           Data.Maybe
import qualified Data.Map as Map

type Program = Map.Map Int Int
type Address = Int
type InstructionPointer = Int
type RelativeBase = Int
type ParamIx = Int
data Mode = Positional | Immediate | Relative
data OpCode = Add | Multiply | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equal | RelativeBaseOffset | Halt
    deriving (Show, Eq)
data ProgramState = ProgramState {
    program :: Program,
    ip :: InstructionPointer,
    inputs :: [Int],
    outputs :: [Int],
    rb :: RelativeBase
} deriving (Show, Eq)
data Point = Point { x :: Int, y :: Int }
    deriving (Show, Eq, Ord)
data Heading = Up | Down | Left | Right
type Drawing = Map.Map Point Int

answer1 :: Program -> Int
answer1 program =
    Map.size
        $ runRobot (initialState program) Point { x = 0, y = 0 } Up
        $ Map.fromList []

answer2 :: Program -> Drawing
answer2 program =
    runRobot (initialState program) Point { x = 0, y = 0 } Up
        $ Map.fromList [(Point { x = 0, y = 0 }, 1)]

runRobot :: ProgramState -> Point -> Heading -> Drawing -> Drawing
runRobot state pos heading drawing =
    case run state { inputs = [fromMaybe 0 $ Map.lookup pos drawing] } of
        (Halt,  _) -> drawing
        (_, newState1) -> case run newState1 of
            (Halt,  _) -> error "Halted between outputs"
            (Input, _) -> error "More input required"
            (Output, outputState) -> 
                let (direction:color:_) = outputs outputState
                    newHeading = turn heading direction
                    newPos = moveForward newHeading pos
                in  runRobot outputState newPos newHeading
                        $ Map.insert pos color drawing

turn :: Heading -> Int -> Heading
turn Up        0 = Lib.Left
turn Lib.Left  0 = Down
turn Down      0 = Lib.Right
turn Lib.Right 0 = Up
turn Up        1 = Lib.Right
turn Lib.Right 1 = Down
turn Down      1 = Lib.Left
turn Lib.Left  1 = Up

moveForward :: Heading -> Point -> Point
moveForward Up        pos = pos { y = y pos - 1 }
moveForward Down      pos = pos { y = y pos + 1 }
moveForward Lib.Left  pos = pos { x = x pos - 1 }
moveForward Lib.Right pos = pos { x = x pos + 1 }

run :: ProgramState -> (OpCode, ProgramState)
run state = case opCode state of
    Add -> binaryOp (+) state
    Multiply -> binaryOp (*) state
    LessThan -> binaryOp (\p1 p2 -> if p1 < p2 then 1 else 0) state
    Equal -> binaryOp (\p1 p2 -> if p1 == p2 then 1 else 0) state
    JumpIfTrue -> jumpIf (/= 0) state
    JumpIfFalse -> jumpIf (== 0) state
    RelativeBaseOffset -> run state { ip = ip state + 2, rb = rb state + param 1 state }
    Input -> case inputs state of
        (input:rest) -> run state { program = write 1 state input, ip = ip state + 2, inputs = rest }
        [] -> (Input, state)
    Output -> (Output, state { ip = ip state + 2, outputs = param 1 state : outputs state })
    Halt -> (Halt, state)

binaryOp :: (Int -> Int -> Int) -> ProgramState -> (OpCode, ProgramState)
binaryOp op state = run state { program = write 3 state $ op (param 1 state) (param 2 state), ip = ip state + 4 }

jumpIf :: (Int -> Bool) -> ProgramState -> (OpCode, ProgramState)
jumpIf pred state = run state { ip = if pred (param 1 state) then param 2 state else ip state + 3 }
                
initialState :: Program -> ProgramState
initialState program = ProgramState { program = program, ip = 0, rb = 0, inputs = [], outputs = [] }

readRelative :: Int -> ProgramState -> Int
readRelative offset state = Lib.read (ip state + offset) state

read :: Address -> ProgramState -> Int
read addr state = fromMaybe 0 $ Map.lookup addr (program state)

write :: ParamIx -> ProgramState -> Int -> Program
write paramIx state val = Map.insert (paramAddr paramIx state) val (program state)

param :: ParamIx -> ProgramState -> Int
param paramIx state = Lib.read (paramAddr paramIx state) state

paramAddr :: ParamIx -> ProgramState -> Address 
paramAddr paramIx state = case paramMode paramIx state of
    Positional -> readRelative paramIx state
    Immediate -> ip state + paramIx
    Relative -> rb state + readRelative paramIx state

paramMode :: ParamIx -> ProgramState -> Mode
paramMode paramIx = parseParamMode paramIx . readRelative 0

parseParamMode :: ParamIx -> Int -> Mode
parseParamMode paramIx i = case (i `div` (10 ^ (paramIx + 1))) `mod` 10 of
    0 -> Positional
    1 -> Immediate
    2 -> Relative

opCode :: ProgramState -> OpCode
opCode = parseOpCode . readRelative 0

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
