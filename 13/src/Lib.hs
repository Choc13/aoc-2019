module Lib
    ( module Lib
    )
where

import Data.List
import Data.List.Split
import           Data.Maybe
import qualified Data.Map as Map
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

type Tiles = Map.Map (Int, Int) Int

answer1 :: Program -> Int
answer1 = length . filter (== 2) . Map.elems . createTiles . createArcade . initialState

createTiles :: ProgramState -> Tiles
createTiles = Map.fromList . map (\(x:y:id:_) -> ((x, y), id)) . chunksOf 3 . reverse . outputs

createArcade :: ProgramState -> ProgramState
createArcade state =
    case run state of
        (Halt,  out) -> out
        (Input, _) -> error "Shouldn't need any input"
        (Output, newState) -> createArcade newState

play :: ProgramState -> IO ()
play state = case run state of
    (Halt, newState) -> do
        let tiles = createTiles newState
        putStrLn $ "You win! Score: " ++ show (fromMaybe 0 $ Map.lookup (-1, 0) tiles)
    (Input, newState) -> do
        let tiles = createTiles newState
        putStrLn $ "Score: " ++ show (fromMaybe 0 $ Map.lookup (-1, 0) tiles)
        putStrLn $ unlines $ draw tiles
        input <- getLine
        play newState { inputs = [Prelude.read input] }
    (Output, newState) -> play newState

automateInput :: Tiles -> Int
automateInput tiles =
    let 
        ball = head $ Map.keys $ Map.filter (== 4) tiles
        paddle = head $ Map.keys $ Map.filter (== 3) tiles
    in signum (fst ball - fst paddle)

draw :: Tiles -> [String]
draw tiles =
    let points = Map.keysSet tiles
        maxY = snd $ maximumBy (comparing snd) points
        maxX = fst $ maximumBy (comparing fst) points
    in [ [ drawTile $ fromMaybe 0 $ Map.lookup (col, row) tiles | col <- [0..maxX] ] | row <- [0..maxY] ]
    
drawTile :: Int -> Char
drawTile tileId = case tileId of
    0 -> ' '
    1 -> '#'
    2 -> '█'
    3 -> '_'
    4 -> 'o'

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
