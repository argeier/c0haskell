module Compile.X86 (
    generateX86,
) where

import Control.Monad (unless, when)
import Control.Monad.State (
    MonadState (get, put),
    State,
    execState,
    modify,
 )
import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as Map
import Text.Printf (printf)

data Operand
    = Reg String
    | Mem String
    | Imm String
    deriving (Show, Eq)

showOperand :: Operand -> String
showOperand (Reg r) = r
showOperand (Mem m) = printf "QWORD PTR %s" m
showOperand (Imm i) = i

data RegState = RegState
    { regMap :: Map.Map String Operand
    , freeRegs :: [String]
    , stackSlots :: Int
    , instructions :: [String]
    }

type RegAlloc a = State RegState a

initialState :: RegState
initialState =
    RegState
        { regMap = Map.empty
        , freeRegs = ["rbx", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
        , stackSlots = 0
        , instructions = []
        }

generateX86 :: [String] -> [String]
generateX86 aasm = [".intel_syntax noprefix"] ++ prologue ++ body ++ epilogue
  where
    finalState = execState (processAAsmInstructions aasm) initialState
    stackSize = stackSlots finalState
    body = reverse $ instructions finalState

    prologue =
        [ ".global main"
        , ".text"
        , "main:"
        , "\tcall _main"
        , "\tmov rdi, rax"
        , "\tmov rax, 60"
        , "\tsyscall"
        , "_main:"
        , "\tpush rbp"
        , "\tmov rbp, rsp"
        ]
            ++ ([printf "\tsub rsp, %d" (8 * stackSize) | stackSize > 0])

    epilogue =
        [ "\tmov rsp, rbp"
        , "\tpop rbp"
        , "\tret"
        ]

emit :: String -> RegAlloc ()
emit instr = modify $ \s -> s{instructions = instr : instructions s}

emit' :: String -> [Operand] -> RegAlloc ()
emit' opcode ops = emit $ printf "\t%s %s" opcode (commaSep (map showOperand ops))

commaSep :: [String] -> String
commaSep = intercalate ", "

getOperand :: String -> RegAlloc Operand
getOperand tempNameStr = do
    st <- get
    case Map.lookup tempNameStr (regMap st) of
        Just operand -> return operand
        Nothing -> do
            case freeRegs st of
                (reg : regs) -> do
                    let newOperand = Reg reg
                    put st{regMap = Map.insert tempNameStr newOperand (regMap st), freeRegs = regs}
                    return newOperand
                [] -> do
                    let newSlot = stackSlots st + 1
                    let memLocation = printf "[rbp-%d]" (8 * newSlot)
                    let newOperand = Mem memLocation
                    put st{regMap = Map.insert tempNameStr newOperand (regMap st), stackSlots = newSlot}
                    return newOperand

processAAsmInstructions :: [String] -> RegAlloc ()
processAAsmInstructions = mapM_ processAAsmInstruction

processAAsmInstruction :: String -> RegAlloc ()
processAAsmInstruction instr
    | "ret " `isPrefixOf` instr = do
        let tempSrcStr = trim $ drop 4 instr
        srcOp <- getOperand tempSrcStr
        emitMove (Reg "rax") srcOp
    | otherwise = case words instr of
        [destTempStr, "=", valStr] | all isDigit valStr || (not (null valStr) && head valStr == '-' && all isDigit (tail valStr)) -> do
            destOp <- getOperand destTempStr
            if valStr == "-2147483648"
                then emitMove destOp (Imm "0x80000000")
                else emitMove destOp (Imm valStr)
        [destTempStr, "=", srcTempStr] -> do
            destOp <- getOperand destTempStr
            srcOp <- getOperand srcTempStr
            emitMove destOp srcOp
        [destTempStr, "=", opStr, src1TempStr] | opStr == "-" -> do
            destOp <- getOperand destTempStr
            src1Op <- getOperand src1TempStr
            emitMove destOp src1Op
            emitUnaryOp "neg" destOp
        [destTempStr, "=", src1TempStr, opStr, src2TempStr] -> do
            destOp <- getOperand destTempStr
            src1Op <- getOperand src1TempStr
            actualSrc2Op <- getOperand src2TempStr
            processBinOp destOp src1Op opStr actualSrc2Op
        [destTempStr, opEqStr, srcTempStr] | last opEqStr == '=' && length opEqStr >= 2 -> do
            let opStr = init opEqStr
            destAsSrc1Op <- getOperand destTempStr
            src2Op <- getOperand srcTempStr
            processBinOp destAsSrc1Op destAsSrc1Op opStr src2Op
        _ -> error $ "Cannot parse AAsm instruction: " ++ instr

processBinOp :: Operand -> Operand -> String -> Operand -> RegAlloc ()
processBinOp destOp src1Op opStr actualSrc2Op =
    case opStr of
        "+" -> emitBinary "add" destOp src1Op actualSrc2Op
        "-" -> emitBinary "sub" destOp src1Op actualSrc2Op
        "*" -> emitBinary "imul" destOp src1Op actualSrc2Op
        "/" -> do
            emitMove (Reg "rax") src1Op
            let divisorFinalOp = actualSrc2Op
            when (divisorFinalOp == Reg "rax" || divisorFinalOp == Reg "rdx") $
                error $
                    "Division divisor " ++ show divisorFinalOp ++ " conflicts with rax/rdx usage."

            emit "\tpush rdx"
            emit "\tcqo"
            emitDivIdiv divisorFinalOp
            emitMove destOp (Reg "rax")
            emit "\tpop rdx"
        "%" -> do
            emitMove (Reg "rax") src1Op
            let divisorFinalOp = actualSrc2Op
            when (divisorFinalOp == Reg "rax" || divisorFinalOp == Reg "rdx") $
                error $
                    "Modulus divisor " ++ show divisorFinalOp ++ " conflicts with rax/rdx usage for idiv."

            emit "\tpush rdx"
            emit "\tcdq"
            emitDivIdiv divisorFinalOp
            emitMove destOp (Reg "rdx")
            emit "\tpop rdx"
        _ -> error $ "Unsupported binary AAsm operator: " ++ opStr

emitMove :: Operand -> Operand -> RegAlloc ()
emitMove dest@(Reg _) src@(Imm _) = emit' "mov" [dest, src]
emitMove dest@(Mem _) src@(Imm _) = emit' "mov" [dest, src]
emitMove dest@(Reg destReg) src@(Reg srcReg)
    | destReg == srcReg = return ()
    | otherwise = emit' "mov" [dest, src]
emitMove dest@(Reg _) src@(Mem _) = emit' "mov" [dest, src]
emitMove dest@(Mem _) src@(Reg _) = emit' "mov" [dest, src]
emitMove dest@(Mem _) src@(Mem _) = do
    emit "\tpush rax"
    emit' "mov" [Reg "rax", src]
    emit' "mov" [dest, Reg "rax"]
    emit "\tpop rax"
emitMove d s = error $ "Unsupported move combination: " ++ show d ++ " <- " ++ show s

emitUnaryOp :: String -> Operand -> RegAlloc ()
emitUnaryOp op destOp = emit' op [destOp]

emitBinary :: String -> Operand -> Operand -> Operand -> RegAlloc ()
emitBinary opName destOp src1Op src2Op = do
    if opName == "imul"
        then do
            emit "\tpush rax"
            emit "\tpush rdx"

            emitMove (Reg "rax") src1Op

            case src2Op of
                Imm _ -> emit' "imul" [Reg "rax", src2Op]
                Reg _ -> emit' "imul" [Reg "rax", src2Op]
                Mem _ -> emit' "imul" [Reg "rax", src2Op]

            emitMove destOp (Reg "rax")

            emit "\tpop rdx"
            emit "\tpop rax"
        else do
            unless (destOp == src1Op) $ do
                emitMove destOp src1Op

            case (destOp, src2Op) of
                (Reg _, Reg _) -> emit' opName [destOp, src2Op]
                (Reg _, Mem _) -> emit' opName [destOp, src2Op]
                (Mem _, Reg _) -> emit' opName [destOp, src2Op]
                (Mem _, Mem _) -> do
                    emit "\tpush rax"
                    emitMove (Reg "rax") src2Op
                    emit' opName [destOp, Reg "rax"]
                    emit "\tpop rax"
                (Mem _, Imm _) -> emit' opName [destOp, src2Op]
                (Reg _, Imm _) -> emit' opName [destOp, src2Op]
                _ -> error $ "Unsupported binary operand combination for " ++ opName ++ ": " ++ show destOp ++ ", " ++ show src2Op

emitDivIdiv :: Operand -> RegAlloc ()
emitDivIdiv divisorOp = emit' "idiv" [divisorOp]

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse