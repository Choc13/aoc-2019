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
answer1 program = sum $ map (\p -> x p * y p) $ Set.elems $ intersections $ createMap $ initialState program

answer2 :: Program -> Int
answer2 program = 
    let a = "L,12,L,8,L,8\n"
        b = "L,12,R,4,L,12,R,6\n"
        c = "R,4,L,12,L,12,R,6\n"
        routine = "A,B,A,C,B,A,C,A,C,B\n"
        feed = "n\n"
        inputs = map ord $ concat [routine, a, b, c, feed]
    in findRobots $ (initialState program) { inputs = inputs }

findRobots :: ProgramState -> Int
findRobots state = case run state of
    (Halt, finalState) -> head $ outputs finalState
    (Output, next) -> findRobots next
    (_, _) -> error "Shouldn't need more input"

intersections :: Map.Map Point Char -> Set.Set Point
intersections m =
    let path = Map.keysSet $ Map.filter (== '#') m
    in Set.filter (isIntersection path) path
                
isIntersection :: Set.Set Point -> Point -> Bool
isIntersection path p = Set.fromList (neighbours p) `Set.isSubsetOf` path

neighbours :: Point -> [Point]
neighbours pos = [
    pos { y = y pos - 1 },
    pos { y = y pos + 1 },
    pos { x = x pos - 1 },
    pos { x = x pos + 1 }]

createMap :: ProgramState -> Map.Map Point Char
createMap state = case run state of
    (Halt, newState) -> Map.map chr
        $ Map.fromList
        $ createPoints
        $ splitOn [10]
        $ reverse
        $ outputs newState
    (Output, newState) -> createMap newState
    (Input, _) -> error "Program should not halt or require more input"

createPoints :: [[Int]] -> [(Point, Int)]
createPoints rows = do
    (y, row) <- zip [0..] rows
    (x, val) <- zip [0..] row
    pure (Point { x = x, y = y }, val)
                
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
