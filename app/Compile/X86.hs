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
import Data.List (intercalate, isPrefixOf, isSuffixOf)
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
    -- Handle labels (e.g., "label_name:")
    | ":" `isSuffixOf` instr = do
        emit $ instr  -- Just emit the label as-is
    
    -- Handle unconditional jumps (e.g., "goto label_name")
    | "goto " `isPrefixOf` instr = do
        let label = trim $ drop 5 instr
        emit $ "\tjmp " ++ label
    
    -- Handle conditional jumps (e.g., "if %1 == 0 goto label_name")
    | "if " `isPrefixOf` instr = do
        let rest = trim $ drop 3 instr
        case parseCondJump rest of
            Just (tempReg, label) -> do
                regOp <- getOperand tempReg
                -- Compare register content with 0 and jump if equal
                emit' "cmp" [regOp, Imm "0"]
                emit $ "\tje " ++ label
            Nothing -> error $ "Cannot parse conditional jump: " ++ instr
    
    -- Handle return statements
    | "ret " `isPrefixOf` instr = do
        let tempSrcStr = trim $ drop 4 instr
        srcOp <- getOperand tempSrcStr
        emitMove (Reg "rax") srcOp
    
    -- Handle regular assignments and operations
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
        [destTempStr, "=", opStr, src1TempStr] | opStr `elem` unaryOps -> do
            destOp <- getOperand destTempStr
            src1Op <- getOperand src1TempStr
            processUnaryOp destOp opStr src1Op
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

-- Parse conditional jump format: "%reg == 0 goto label"
parseCondJump :: String -> Maybe (String, String)
parseCondJump s = case words s of
    [reg, "==", "0", "goto", label] -> Just (reg, label)
    _ -> Nothing

-- List of unary operators
unaryOps :: [String]
unaryOps = ["-", "!", "~"]

-- List of binary operators  
binaryOps :: [String]
binaryOps = ["+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "&&", "||", "&", "^", "|", "<<", ">>"]

processUnaryOp :: Operand -> String -> Operand -> RegAlloc ()
processUnaryOp destOp opStr srcOp = do
    case opStr of
        "-" -> do
            emitMove destOp srcOp
            emitUnaryOp "neg" destOp
        "!" -> do
            -- Logical NOT: convert non-zero to 0, zero to 1
            emitMove destOp srcOp
            emit' "cmp" [destOp, Imm "0"]
            emit "\tsete al"  -- Set AL to 1 if zero, 0 otherwise
            emit "\tmovzx eax, al"  -- Zero-extend AL to EAX
            emitMove destOp (Reg "rax")
        "~" -> do
            emitMove destOp srcOp
            emitUnaryOp "not" destOp
        _ -> error $ "Unsupported unary operator: " ++ opStr

processBinOp :: Operand -> Operand -> String -> Operand -> RegAlloc ()
processBinOp destOp src1Op opStr actualSrc2Op =
    case opStr of
        "+" -> emitBinaryFinal "add" destOp src1Op actualSrc2Op
        "-" -> emitBinaryFinal "sub" destOp src1Op actualSrc2Op
        "*" -> emitBinaryFinal "imul" destOp src1Op actualSrc2Op
        "/" -> emitDivMod "div" destOp src1Op actualSrc2Op
        "%" -> emitDivMod "mod" destOp src1Op actualSrc2Op
        -- Comparison operators
        "<" -> emitComparison "setl" destOp src1Op actualSrc2Op
        "<=" -> emitComparison "setle" destOp src1Op actualSrc2Op
        ">" -> emitComparison "setg" destOp src1Op actualSrc2Op
        ">=" -> emitComparison "setge" destOp src1Op actualSrc2Op
        "==" -> emitComparison "sete" destOp src1Op actualSrc2Op
        "!=" -> emitComparison "setne" destOp src1Op actualSrc2Op
        -- Logical operators (short-circuit evaluation already handled in AAsm)
        "&&" -> emitLogicalAnd destOp src1Op actualSrc2Op
        "||" -> emitLogicalOr destOp src1Op actualSrc2Op
        -- Bitwise operators
        "&" -> emitBinaryFinal "and" destOp src1Op actualSrc2Op
        "^" -> emitBinaryFinal "xor" destOp src1Op actualSrc2Op
        "|" -> emitBinaryFinal "or" destOp src1Op actualSrc2Op
        -- Shift operators
        "<<" -> emitShift "sal" destOp src1Op actualSrc2Op
        ">>" -> emitShift "sar" destOp src1Op actualSrc2Op
        _ -> error $ "Unsupported binary AAsm operator: " ++ opStr

-- Handle division and modulo
emitDivMod :: String -> Operand -> Operand -> Operand -> RegAlloc ()
emitDivMod op destOp src1Op src2Op = do
    emit "\tpush rdx"
    emitMove (Reg "rax") src1Op

    let prepareDivisor = case src2Op of
            Reg rName | rName == "rax" || rName == "rdx" -> do
                emitMove (Reg "rcx") src2Op
                return (Reg "rcx")
            Reg _ -> return src2Op
            Mem _ -> return src2Op
            Imm _ -> do
                emitMove (Reg "rcx") src2Op
                return (Reg "rcx")

    divisorOp <- prepareDivisor
    emit "\tcdq"
    emit' "idiv" [divisorOp]
    
    case op of
        "div" -> emitMove destOp (Reg "rax")  -- Quotient in RAX
        "mod" -> emitMove destOp (Reg "rdx")  -- Remainder in RDX
        _ -> error $ "Unknown div/mod operation: " ++ op
    
    emit "\tpop rdx"

-- Handle comparison operations
emitComparison :: String -> Operand -> Operand -> Operand -> RegAlloc ()
emitComparison setInstr destOp src1Op src2Op = do
    -- Always use a temporary register for the comparison to avoid conflicts
    let tempReg = Reg "rax"
    
    emitMove tempReg src1Op
    case src2Op of
        Reg _ -> emit' "cmp" [tempReg, src2Op]
        Mem _ -> emit' "cmp" [tempReg, src2Op]  
        Imm _ -> emit' "cmp" [tempReg, src2Op]
    
    -- Set AL based on comparison, then zero-extend to full register
    emit $ "\t" ++ setInstr ++ " al"
    emit "\tmovzx eax, al"
    
    -- Move result to destination
    emitMove destOp (Reg "rax")

-- Handle logical AND (both operands already evaluated)
emitLogicalAnd :: Operand -> Operand -> Operand -> RegAlloc ()
emitLogicalAnd destOp src1Op src2Op = do
    let calcReg = case destOp of
            Reg _ -> destOp  
            _ -> Reg "rax"
    
    -- Convert operands to 0/1 then multiply
    emitMove calcReg src1Op
    emit' "cmp" [calcReg, Imm "0"]
    emit "\tsetne al"
    emit "\tmovzx eax, al"
    emitMove calcReg (Reg "rax")
    
    let tempReg = Reg "rcx"
    emitMove tempReg src2Op
    emit' "cmp" [tempReg, Imm "0"]
    emit "\tsetne cl"
    emit "\tmovzx ecx, cl"
    
    emit' "imul" [calcReg, tempReg]
    
    case destOp of
        Mem _ -> emitMove destOp calcReg
        _ -> return ()

-- Handle logical OR (both operands already evaluated)
emitLogicalOr :: Operand -> Operand -> Operand -> RegAlloc ()
emitLogicalOr destOp src1Op src2Op = do
    let calcReg = case destOp of
            Reg _ -> destOp
            _ -> Reg "rax"
    
    -- OR the operands, then convert result to 0/1
    emitMove calcReg src1Op
    case src2Op of
        Reg _ -> emit' "or" [calcReg, src2Op]
        Mem _ -> emit' "or" [calcReg, src2Op]
        Imm _ -> emit' "or" [calcReg, src2Op]
    
    emit' "cmp" [calcReg, Imm "0"]
    emit "\tsetne al"
    emit "\tmovzx eax, al"
    emitMove calcReg (Reg "rax")
    
    case destOp of
        Mem _ -> emitMove destOp calcReg
        _ -> return ()

-- Handle shift operations
emitShift :: String -> Operand -> Operand -> Operand -> RegAlloc ()
emitShift shiftInstr destOp src1Op src2Op = do
    let calcReg = case destOp of
            Reg _ -> destOp
            _ -> Reg "rax"
    
    emitMove calcReg src1Op
    
    -- Shift amount must be in CL register or immediate
    case src2Op of
        Imm i -> emit' shiftInstr [calcReg, Imm i]
        _ -> do
            emit "\tpush rcx"
            emitMove (Reg "rcx") src2Op
            emit' shiftInstr [calcReg, Reg "cl"]  -- Use CL (low 8 bits of RCX)
            emit "\tpop rcx"
    
    case destOp of
        Mem _ -> emitMove destOp calcReg
        _ -> return ()

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
            (Reg dReg, Reg s1Reg, Imm imm) ->
                emit' "imul" [Reg dReg, Reg s1Reg, Imm imm]
            (Reg dReg, Mem s1Mem, Imm imm) ->
                emit' "imul" [Reg dReg, Mem s1Mem, Imm imm]
            _ -> generateTwoOperandSequence
        else
            generateTwoOperandSequence
  where
    generateTwoOperandSequence = do
        let calcReg = case destOp of
                Reg _ -> destOp
                _ -> Reg "rax"

        when (calcReg /= src1Op) $
            emitMove calcReg src1Op
        case src2Op of
            Reg _ -> emit' opName [calcReg, src2Op]
            Mem _ -> emit' opName [calcReg, src2Op]
            Imm _ -> emit' opName [calcReg, src2Op]

        case destOp of
            Mem _ ->
                when (calcReg == Reg "rax") $
                    emitMove destOp (Reg "rax")
            _ -> return ()

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse