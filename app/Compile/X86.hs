module Compile.X86 (
    generateX86,
) where

import Control.Monad.State
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map as Map
import Text.Printf

data RegState = RegState
    { regMap :: Map.Map String String
    , regType :: Map.Map String RegType
    , freeRegs :: [String]
    , stackSlots :: Int
    , instructions :: [String]
    }

data RegType = Register | Memory
    deriving (Eq)

type RegAlloc a = State RegState a

initialState :: RegState
initialState =
    RegState
        { regMap = Map.empty
        , regType = Map.empty
        , freeRegs = ["rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
        , stackSlots = 0
        , instructions = []
        }

generateX86 :: [String] -> [String]
generateX86 aasm = [".intel_syntax noprefix"] ++ prologue ++ body ++ epilogue
  where
    (_, regState) = runState (processInstructions aasm) initialState
    stackSize = stackSlots regState
    body = instructions regState

    prologue =
        [ ".global main"
        , ".global _main"
        , ".text"
        , "main:"
        , "\tcall _main"
        , "\tmov rdi, rax"
        , "\tmov rax, 0x3C"
        , "\tsyscall"
        , "_main:"
        , "\tpush rbp"
        , "\tmov rbp, rsp"
        ]
            ++ stackAdjust

    stackAdjust =
        if stackSize > 0
            then [printf "\tsub rsp, %d" (8 * stackSize)]
            else []

    epilogue =
        [ "\tmov rsp, rbp"
        , "\tpop rbp"
        , "\tret"
        ]

processInstructions :: [String] -> RegAlloc ()
processInstructions [] = return ()
processInstructions (instr : instrs) = do
    processInstruction instr
    processInstructions instrs

processInstruction :: String -> RegAlloc ()
processInstruction instr
    | "ret " `isPrefixOf` instr = do
        let reg = drop 4 instr
        preg <- getReg reg
        emitMove "rax" preg
    | " = " `isInfixOf` instr = do
        let (dest, rest) = break (== '=') instr
        let destReg = trim dest
        let expr = trim (drop 1 rest)
        processExpr destReg expr
    | any (`isInfixOf` instr) [" += ", " -= ", " *= ", " /= ", " %= "] = do
        -- Handle compound assignments like x += y, x /= y, etc.
        let tokens = words instr
        let destReg = tokens !! 0
        let op = init (tokens !! 1) -- Remove the '=' from the operator
        let srcReg = tokens !! 2
        pdest <- getReg destReg
        psrc <- getReg srcReg

        case op of
            "/" -> do
                emit "\tpush rdx"
                emitMove "rax" pdest
                emit "\tcdq" -- Sign-extend rax into rdx:rax
                emitDiv psrc
                emitMove pdest "rax"
                emit "\tpop rdx"
            "%" -> do
                emit "\tpush rdx"
                emitMove "rax" pdest
                emit "\tcdq" -- Sign-extend rax into rdx:rax
                emitDiv psrc
                emitMove pdest "rdx"
                emit "\tpop rdx"
            _ -> emitBinOp op pdest psrc
    | otherwise = emit (printf "\t%s" instr)

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

processExpr :: String -> String -> RegAlloc ()
processExpr dest expr
    | all isDigit expr = do
        pdest <- allocReg dest
        emitMoveImm pdest expr
    | head expr == '-' && all isDigit (tail expr) = do
        pdest <- allocReg dest
        if expr == "-2147483648"
            then
                emitMoveImm pdest "0x80000000"
            else do
                emitMoveImm pdest (tail expr)
                emitUnaryOp "neg" pdest
    | any (`isInfixOf` expr) [" + ", " - ", " * ", " / ", " % "] = do
        let (left, op, right) = parseBinOp expr
        pleft <- getReg left
        pright <- getReg right
        pdest <- allocReg dest

        case op of
            "*" ->
                if right == "1" && (pleft == "-1" || left == "1" && pright == "-1")
                    then do
                        if pright == "-1"
                            then do
                                emitMove pdest pleft
                                emitUnaryOp "neg" pdest
                            else do
                                emitMove pdest pright
                                emitUnaryOp "neg" pdest
                    else do
                        emitMove pdest pleft
                        emitBinOp op pdest pright
            "/" -> do
                emit "\tpush rdx"
                emitMove "rax" pleft
                emit "\tcdq" -- Sign-extend rax into rdx:rax
                emitDiv pright
                emitMove pdest "rax"
                emit "\tpop rdx"
            "%" -> do
                emit "\tpush rdx"
                emitMove "rax" pleft
                emit "\tcdq" -- Sign-extend rax into rdx:rax
                emitDiv pright
                emitMove pdest "rdx"
                emit "\tpop rdx"
            _ -> do
                emitMove pdest pleft
                emitBinOp op pdest pright
    | "-" `isPrefixOf` expr = do
        let operand = trim (drop 1 expr)
        poperand <- getReg operand
        pdest <- allocReg dest
        emitMove pdest poperand
        emitUnaryOp "neg" pdest
    | otherwise = do
        psrc <- getReg expr
        pdest <- allocReg dest
        emitMove pdest psrc

parseBinOp :: String -> (String, String, String)
parseBinOp expr =
    let tokens = words expr
        left = tokens !! 0
        op = tokens !! 1
        right = tokens !! 2
     in (left, op, right)

translateOp :: String -> String
translateOp "+" = "add"
translateOp "-" = "sub"
translateOp "*" = "imul"
translateOp _ = error "Unsupported operation"

-- Extract register number from virtual register name (e.g., "%0" -> "0")
extractRegNum :: String -> String
extractRegNum reg =
    if head reg == '%'
        then drop 1 reg
        else reg

allocReg :: String -> RegAlloc String
allocReg vreg = do
    let vregName = extractRegNum vreg
    state <- get
    case Map.lookup vregName (regMap state) of
        Just preg -> return preg
        Nothing -> case freeRegs state of
            (reg : regs) -> do
                put
                    state
                        { regMap = Map.insert vregName reg (regMap state)
                        , regType = Map.insert vregName Register (regType state)
                        , freeRegs = regs
                        }
                return reg
            [] -> do
                let slot = stackSlots state
                let stackLoc = printf "[rbp-%d]" (8 * (slot + 1))
                put
                    state
                        { regMap = Map.insert vregName stackLoc (regMap state)
                        , regType = Map.insert vregName Memory (regType state)
                        , stackSlots = slot + 1
                        }
                return stackLoc

getReg :: String -> RegAlloc String
getReg vreg = do
    let vregName = extractRegNum vreg
    state <- get
    case Map.lookup vregName (regMap state) of
        Just preg -> return preg
        Nothing -> allocReg vreg

isMemory :: String -> RegAlloc Bool
isMemory vreg = do
    let vregName = extractRegNum vreg
    state <- get
    case Map.lookup vregName (regType state) of
        Just Memory -> return True
        _ -> return False

emitMove :: String -> String -> RegAlloc ()
emitMove dest src = do
    destIsMem <- isMemLocOrSpilled dest
    srcIsMem <- isMemLocOrSpilled src

    case (destIsMem, srcIsMem) of
        (True, True) -> do
            -- Memory-to-memory moves require a temporary register
            emit "\tpush rax"
            emit (printf "\tmov rax, QWORD PTR %s" src)
            emit (printf "\tmov QWORD PTR %s, rax" dest)
            emit "\tpop rax"
        (True, False) ->
            emit (printf "\tmov QWORD PTR %s, %s" dest src)
        (False, True) ->
            emit (printf "\tmov %s, QWORD PTR %s" dest src)
        (False, False) ->
            emit (printf "\tmov %s, %s" dest src)

emitMoveImm :: String -> String -> RegAlloc ()
emitMoveImm dest imm = do
    destIsMem <- isMemLocOrSpilled dest
    if destIsMem
        then emit (printf "\tmov QWORD PTR %s, %s" dest imm)
        else emit (printf "\tmov %s, %s" dest imm)

emitBinOp :: String -> String -> String -> RegAlloc ()
emitBinOp op dest src = do
    destIsMem <- isMemLocOrSpilled dest
    srcIsMem <- isMemLocOrSpilled src

    let opName = translateOp op

    case (destIsMem, srcIsMem, op) of
        -- Special case for imul with memory operands
        (True, _, "*") -> do
            -- When destination is memory, we need a temporary register
            emit "\tpush rax"
            emit (printf "\tmov rax, QWORD PTR %s" dest)
            if srcIsMem
                then emit (printf "\timul rax, QWORD PTR %s" src)
                else emit (printf "\timul rax, %s" src)
            emit (printf "\tmov QWORD PTR %s, rax" dest)
            emit "\tpop rax"
        (True, True, _) -> do
            -- Memory-to-memory operations require a temporary register
            emit "\tpush rax"
            emit (printf "\tmov rax, QWORD PTR %s" src)
            emit (printf "\t%s QWORD PTR %s, rax" opName dest)
            emit "\tpop rax"
        (True, False, _) ->
            emit (printf "\t%s QWORD PTR %s, %s" opName dest src)
        (False, True, _) ->
            emit (printf "\t%s %s, QWORD PTR %s" opName dest src)
        (False, False, _) ->
            emit (printf "\t%s %s, %s" opName dest src)

emitUnaryOp :: String -> String -> RegAlloc ()
emitUnaryOp op dest = do
    destIsMem <- isMemLocOrSpilled dest
    if destIsMem
        then emit (printf "\t%s QWORD PTR %s" op dest)
        else emit (printf "\t%s %s" op dest)

emitDiv :: String -> RegAlloc ()
emitDiv divisor = do
    divisorIsMem <- isMemLocOrSpilled divisor
    if divisorIsMem
        then emit (printf "\tidiv QWORD PTR %s" divisor)
        else emit (printf "\tidiv %s" divisor)

isMemLocOrSpilled :: String -> RegAlloc Bool
isMemLocOrSpilled loc = return $ '[' `elem` loc

emit :: String -> RegAlloc ()
emit instr = do
    state <- get
    put state{instructions = instructions state ++ [instr]}