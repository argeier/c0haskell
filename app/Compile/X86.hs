module Compile.X86 (
    generateX86,
) where

import Control.Monad (when)
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

to32BitRegName :: String -> String
to32BitRegName "rax" = "eax"
to32BitRegName "rbx" = "ebx"
to32BitRegName "rcx" = "ecx"
to32BitRegName "rdx" = "edx"
to32BitRegName "rsi" = "esi"
to32BitRegName "rdi" = "edi"
to32BitRegName "r8" = "r8d"
to32BitRegName "r9" = "r9d"
to32BitRegName "r10" = "r10d"
to32BitRegName "r11" = "r11d"
to32BitRegName "r12" = "r12d"
to32BitRegName "r13" = "r13d"
to32BitRegName "r14" = "r14d"
to32BitRegName "r15" = "r15d"
to32BitRegName "rbp" = "ebp"
to32BitRegName "rsp" = "esp"
to32BitRegName other = other

showOperand :: Operand -> String
showOperand (Reg r) = to32BitRegName r
showOperand (Mem m) = printf "DWORD PTR %s" m
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
    stackRequiredBytes = stackSlots finalState * 8
    alignedStackBytes = if stackRequiredBytes > 0 then ((stackRequiredBytes + 15) `div` 16) * 16 else 0

    body = reverse $ instructions finalState

    prologue =
        [ ".global main"
        , ".text"
        , "main:"
        , "\tcall _main"
        , "\tmov edi, eax"
        , "\tmov rax, 60"
        , "\tsyscall"
        , "_main:"
        , "\tpush rbp"
        , "\tmov rbp, rsp"
        ]
            ++ ([printf "\tsub rsp, %d" alignedStackBytes | alignedStackBytes > 0])

    epilogue =
        ([printf "\tadd rsp, %d" alignedStackBytes | alignedStackBytes > 0])
            ++ [ "\tpop rbp"
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
                (reg64Name : regs) -> do
                    let newOperand = Reg reg64Name
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
        "+" -> emitBinaryFinal "add" destOp src1Op actualSrc2Op
        "-" -> emitBinaryFinal "sub" destOp src1Op actualSrc2Op
        "*" -> emitBinaryFinal "imul" destOp src1Op actualSrc2Op
        "/" -> do
            emit "\tpush rdx"
            emitMove (Reg "rax") src1Op

            let prepareDivisor = case actualSrc2Op of
                    Reg rName | rName == "rax" || rName == "rdx" -> do
                        emitMove (Reg "rcx") actualSrc2Op
                        return (Reg "rcx")
                    Reg _ -> return actualSrc2Op
                    Mem _ -> return actualSrc2Op
                    Imm _ -> do
                        emitMove (Reg "rcx") actualSrc2Op
                        return (Reg "rcx")

            divisorOp <- prepareDivisor
            emit "\tcdq"
            emit' "idiv" [divisorOp]
            emitMove destOp (Reg "rax")
            emit "\tpop rdx"
        "%" -> do
            emit "\tpush rdx"
            emitMove (Reg "rax") src1Op
            let prepareDivisor = case actualSrc2Op of
                    Reg rName | rName == "rax" || rName == "rdx" -> do
                        emitMove (Reg "rcx") actualSrc2Op
                        return (Reg "rcx")
                    Reg _ -> return actualSrc2Op
                    Mem _ -> return actualSrc2Op
                    Imm _ -> do
                        emitMove (Reg "rcx") actualSrc2Op
                        return (Reg "rcx")

            divisorOp <- prepareDivisor
            emit "\tcdq"
            emit' "idiv" [divisorOp]

            emitMove destOp (Reg "rdx")
            emit "\tpop rdx"
        _ -> error $ "Unsupported binary AAsm operator: " ++ opStr

emitMove :: Operand -> Operand -> RegAlloc ()
emitMove dest@(Reg destReg64) src@(Reg srcReg64)
    | destReg64 == srcReg64 = return ()
    | otherwise = emit' "mov" [dest, src]
emitMove dest@(Reg _) src@(Imm _) = emit' "mov" [dest, src]
emitMove dest@(Reg _) src@(Mem _) = emit' "mov" [dest, src]
emitMove dest@(Mem _) src@(Reg _) = emit' "mov" [dest, src]
emitMove dest@(Mem _) src@(Imm _) = emit' "mov" [dest, src]
emitMove dest@(Mem destMem) src@(Mem srcMem)
    | destMem == srcMem = return ()
    | otherwise = do
        emit "\tpush rax"
        emit' "mov" [Reg "rax", src]
        emit' "mov" [dest, Reg "rax"]
        emit "\tpop rax"
emitMove d s = error $ "Unsupported move combination: " ++ show d ++ " <- " ++ show s

emitUnaryOp :: String -> Operand -> RegAlloc ()
emitUnaryOp opCode operand = emit' opCode [operand]

emitBinaryFinal :: String -> Operand -> Operand -> Operand -> RegAlloc ()
emitBinaryFinal opName destOp src1Op src2Op =
    if opName == "imul"
        then case (destOp, src1Op, src2Op) of
            (Reg dr, Reg s1r, Imm s2i) -> emit' "imul" [Reg dr, Reg s1r, Imm s2i]
            (Reg dr, Mem s1m, Imm s2i) -> emit' "imul" [Reg dr, Mem s1m, Imm s2i]
            _ -> fallthroughToTwoOperand
        else
            fallthroughToTwoOperand
  where
    fallthroughToTwoOperand = do
        when (destOp /= src1Op) $ emitMove destOp src1Op
        case (destOp, src2Op) of
            (Reg _, Reg _) -> emit' opName [destOp, src2Op]
            (Reg _, Mem _) -> emit' opName [destOp, src2Op]
            (Reg _, Imm _) -> emit' opName [destOp, src2Op]
            (Mem _, Reg _) -> emit' opName [destOp, src2Op]
            (Mem _, Imm _) -> emit' opName [destOp, src2Op]
            (Mem _, Mem _) -> do
                emit "\tpush rax"
                emitMove (Reg "rax") src2Op
                emit' opName [destOp, Reg "rax"]
                emit "\tpop rax"
            _ -> error $ "emitBinaryFinal: Unsupported combination for " ++ opName ++ " " ++ show destOp ++ ", " ++ show src2Op

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse