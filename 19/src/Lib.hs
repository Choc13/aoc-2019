module Lib
    ( module Lib
    )
where

import Data.List
import Data.List.Split
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ord (comparing)
import Data.Char

data Point = Point { x :: Int, y :: Int }
    deriving (Show, Eq, Ord)

answer1 :: Program -> Int
answer1 program = length $ filter (isTractorBeam program) $ spaceToSearch 49 49

answer2 :: Program -> (Point, Int)
answer2 program = 
    let 
        topRight = head $ filter (canFitSanta program 100) $ walkRightEdge program Point { x = 2, y = 3 }
        topLeft = topRight { x = x topRight - 99 }
    in (topLeft, (x topLeft * 10000) + y topLeft)

walkRightEdge :: Program -> Point -> [Point]
walkRightEdge program origin = 
    let candidates p = 
            [
                p { x = x p + 1 },
                p { x = x p + 1, y = y p + 1},
                p { y = y p + 1 }
            ]
        nextPoint = head . filter (isTractorBeam program) . candidates
    in unfoldr (\p -> Just (p, nextPoint p)) origin

canFitSanta :: Program -> Int -> Point -> Bool
canFitSanta program size topRight =
    let offset = size - 1
        requiredPoints = [
            topRight, 
            topRight { x = x topRight - offset },
            topRight { y = y topRight + offset },
            topRight { x = x topRight - offset, y = y topRight + offset } ]
    in all (isTractorBeam program) requiredPoints

spaceToSearch :: Int -> Int -> [Point]
spaceToSearch x y = do
    row <- [0..y]
    col <- [0..x]
    pure Point { x = col, y = row }

isTractorBeam :: Program -> Point -> Bool
isTractorBeam program point =
    case run ((initialState program) { inputs = [x point, y point] }) of
        (Input, _) -> error "Shouldn't need more input"
        (_, next) -> head (outputs next) == 1

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
