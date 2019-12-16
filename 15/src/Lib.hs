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

data StatusCode = Wall | Moved | Oxygen
    deriving (Show, Eq)
data Command = North | South | East | West
    deriving (Show, Eq)
data Point = Point { x :: Int, y :: Int }
    deriving (Show, Eq, Ord)
data SearchState = SearchState { visited :: Map.Map Point (Int, ProgramState), frontier :: [Point] }
    deriving (Show, Eq)

answer1 :: Program -> Maybe (Point, Int)
answer1 program = 
    let 
        origin = Point { x = 0, y = 0 }
    in search SearchState { 
        visited = Map.fromList [(origin, (0, initialState program))], 
        frontier = [origin] }

answer2 :: Program -> Point -> Int
answer2 program oxygenPos = 
    let 
        origin = Point { x = 0, y = 0 }
        completeMap = createMap SearchState { 
            visited = Map.fromList [(origin, (0, initialState program))], 
            frontier = [origin] }
    in timeToFill (Map.keysSet $ Map.filter (\v -> snd v /= Wall) completeMap) (Map.singleton oxygenPos 0) [oxygenPos]

timeToFill :: Set.Set Point -> Map.Map Point Int -> [Point] -> Int
timeToFill points visited frontier = 
    case frontier of
        [] -> maximum $ Map.elems visited
        (pos:nextFrontier) ->
            let
                nextPoints = Map.fromList 
                    $ filter (\(p, _) -> Set.member p points && Map.notMember p visited)
                    $ neighbours2 pos 
                    $ visited Map.! pos
            in timeToFill points (Map.union visited nextPoints) (nextFrontier ++ Map.keys nextPoints)

neighbours2 :: Point -> Int -> [(Point, Int)]
neighbours2 pos distance = [
    (pos { y = y pos - 1 }, distance + 1), 
    (pos { y = y pos + 1 }, distance + 1),
    (pos { x = x pos - 1 }, distance + 1),
    (pos { x = x pos + 1 }, distance + 1)]

createMap :: SearchState -> Map.Map Point (Int, StatusCode)
createMap searchState =
    case frontier searchState of
        [] -> Map.map (\v -> (fst v, status $ snd v)) $ visited searchState
        (pos:nextFrontier) ->
            let
                posState = visited searchState Map.! pos
            in case status $ snd posState of
                Wall -> createMap searchState { frontier = nextFrontier }
                _ -> 
                    let 
                        newPoints = Map.fromList 
                            $ filter (\n -> Map.notMember (fst n) $ visited searchState) 
                            $ neighbours pos
                            $ visited searchState Map.! pos
                    in createMap searchState {
                        visited = Map.union (visited searchState) newPoints,
                        frontier = nextFrontier ++ Map.keys newPoints }

search :: SearchState -> Maybe (Point, Int)
search searchState =
    case frontier searchState of
        [] -> Nothing
        (pos:nextFrontier) -> 
            let
                posState = visited searchState Map.! pos
            in case status $ snd posState of
                Wall -> search searchState { frontier = nextFrontier }
                Moved -> 
                    let 
                        newPoints = Map.fromList 
                            $ filter (\n -> Map.notMember (fst n) $ visited searchState) 
                            $ neighbours pos 
                            $ visited searchState Map.! pos
                    in search searchState {
                        visited = Map.union (visited searchState) newPoints,
                        frontier = nextFrontier ++ Map.keys newPoints }
                Oxygen -> Just (pos, fst posState)

neighbours :: Point -> (Int, ProgramState) -> [(Point, (Int, ProgramState))]
neighbours pos (distance, state) = [
    (pos { y = y pos - 1 }, (distance + 1, move state North)),
    (pos { y = y pos + 1 }, (distance + 1, move state South)),
    (pos { x = x pos - 1 }, (distance + 1, move state East)),
    (pos { x = x pos + 1 }, (distance + 1, move state West))]

status :: ProgramState -> StatusCode
status state = case outputs state of
    [] -> Moved
    (head:tail) -> parseStatus head

parseStatus :: Int -> StatusCode
parseStatus 0 = Wall
parseStatus 1 = Moved
parseStatus 2 = Oxygen 

move :: ProgramState -> Command -> ProgramState
move state direction =
    let
        input = case direction of
            North -> 1
            South -> 2
            West -> 3
            East -> 4
    in case run state { inputs = [ input ] } of
        (Output, newState) -> newState
        (_, _) -> error "Program should not halt or require more input"

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
