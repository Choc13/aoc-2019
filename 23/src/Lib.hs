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

type Addr = Int
data Packet = Packet { addr :: Addr, x :: Int, y :: Int }
    deriving (Eq, Ord, Show)

answer1 :: Program -> IO ()
answer1 program =
    let computers = createComputers program 50
    in runComputers 0 computers

runComputer :: ProgramState -> IO ()
runComputer state = case run state of
    (Output, nextState) -> do
        print $ "Output" ++ show (outputs nextState)
    (Input, nextState) -> runComputer nextState


runComputers :: Addr -> Map.Map Addr ProgramState -> IO ()
runComputers addr computers = 
    let nextAddr = (addr + 1) `mod` 50
    in case run $ computers Map.! addr of
        (Output, nextState) -> do
            print $ reverse $ outputs nextState
            case reverse $ outputs nextState of
                [255, x, y] -> print y
                [dest, x, y] -> runComputers nextAddr
                    $ Map.insert addr (nextState { outputs = [] })
                    $ Map.adjust (\old -> old { inputs = inputs old ++ [x, y] }) dest computers
                _ -> runComputers addr $ Map.insert addr nextState computers
        (Input, nextState) -> runComputers nextAddr $ Map.insert addr (nextState { inputs = [-1] }) computers
        (Halt, _) -> error "NICs shouldn't halt"

createComputers :: Program -> Int -> Map.Map Int ProgramState
createComputers program count = Map.fromList 
    $ map (\addr -> (addr, ((initialState program) { inputs = [addr] }))) [0..count - 1]

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
