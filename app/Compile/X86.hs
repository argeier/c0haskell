-- File: X86.hs
module Compile.X86 (
    generateX86,
) where

import Control.Monad.State
import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as Map
import Text.Printf

-- Represents an operand in x86 assembly
data Operand
    = Reg String -- Register, e.g., "rax"
    | Mem String -- Memory location, e.g., "[rbp-8]"
    | Imm String -- Immediate value, e.g., "42"
    deriving (Show, Eq)

-- Formats an operand for assembly output
showOperand :: Operand -> String
showOperand (Reg r) = r
showOperand (Mem m) = printf "QWORD PTR %s" m
showOperand (Imm i) = i

data RegState = RegState
    { regMap :: Map.Map String Operand -- Maps AAsm temp string to x86 Operand (Reg or Mem)
    , freeRegs :: [String]
    , stackSlots :: Int
    , instructions :: [String]
    }

type RegAlloc a = State RegState a

initialState :: RegState
initialState =
    RegState
        { regMap = Map.empty
        , -- RAX and RDX are not in the general pool due to their special roles in idiv, etc.
          freeRegs = ["rbx", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
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
        , "\tcall _main" -- _main returns result in rax
        , "\tmov rdi, rax" -- exit status from rax into rdi (1st argument to syscall)
        , "\tmov rax, 60" -- syscall number for exit (0x3C on Linux)
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

-- Emit an instruction (prepends to list for efficiency, reverse at the end)
emit :: String -> RegAlloc ()
emit instr = modify $ \s -> s{instructions = instr : instructions s}

-- Emit a formatted instruction with an opcode and operands
emit' :: String -> [Operand] -> RegAlloc ()
emit' opcode ops = emit $ printf "\t%s %s" opcode (commaSep (map showOperand ops))

-- Helper to join operand strings with a comma
commaSep :: [String] -> String
commaSep = intercalate ", "

-- Gets the x86 operand for an AAsm temporary, allocating if necessary
getOperand :: String -> RegAlloc Operand
getOperand tempNameStr = do
    state <- get
    case Map.lookup tempNameStr (regMap state) of
        Just operand -> return operand
        Nothing -> do
            -- Allocate new
            case freeRegs state of
                (reg : regs) -> do
                    let newOperand = Reg reg
                    put state{regMap = Map.insert tempNameStr newOperand (regMap state), freeRegs = regs}
                    return newOperand
                [] -> do
                    -- Spill to stack
                    let newSlot = stackSlots state + 1
                    let memLocation = printf "[rbp-%d]" (8 * newSlot)
                    let newOperand = Mem memLocation
                    put state{regMap = Map.insert tempNameStr newOperand (regMap state), stackSlots = newSlot}
                    return newOperand

-- Processes a list of AAsm instructions
processAAsmInstructions :: [String] -> RegAlloc ()
processAAsmInstructions = mapM_ processAAsmInstruction

-- Parses and processes a single AAsm instruction string
processAAsmInstruction :: String -> RegAlloc ()
processAAsmInstruction instr
    | "ret " `isPrefixOf` instr = do
        let tempSrcStr = trim $ drop 4 instr
        srcOp <- getOperand tempSrcStr
        emitMove (Reg "rax") srcOp -- Return value must be in rax
    | otherwise = case words instr of
        -- Assignment: %dest = immediate
        [destTempStr, "=", valStr] | all isDigit valStr || (not (null valStr) && head valStr == '-' && all isDigit (tail valStr)) -> do
            destOp <- getOperand destTempStr
            if valStr == "-2147483648" -- Special case for INT_MIN
                then emitMove destOp (Imm "0x80000000")
                else emitMove destOp (Imm valStr)

        -- Assignment: %dest = %src
        [destTempStr, "=", srcTempStr] -> do
            destOp <- getOperand destTempStr
            srcOp <- getOperand srcTempStr
            emitMove destOp srcOp

        -- Unary operation: %dest = op %src1 (e.g., %dest = - %src1)
        [destTempStr, "=", opStr, src1TempStr] | opStr == "-" -> do
            destOp <- getOperand destTempStr
            src1Op <- getOperand src1TempStr
            emitMove destOp src1Op -- Move src1 to dest first (if different)
            emitUnaryOp "neg" destOp -- Apply operation to dest

        -- Binary operation: %dest = %src1 op %src2
        [destTempStr, "=", src1TempStr, opStr, src2TempStr] -> do
            destOp <- getOperand destTempStr
            src1Op <- getOperand src1TempStr
            actualSrc2Op <- getOperand src2TempStr
            processBinOp destOp src1Op opStr actualSrc2Op

        -- Compound assignment: %dest op= %src (e.g. %dest += %src)
        [destTempStr, opEqStr, srcTempStr] | last opEqStr == '=' && length opEqStr >= 2 -> do
            let opStr = init opEqStr -- Remove trailing '=' to get the op (e.g., "+", "*")
            destAsSrc1Op <- getOperand destTempStr -- For "x += y", x is both src1 and dest
            src2Op <- getOperand srcTempStr
            processBinOp destAsSrc1Op destAsSrc1Op opStr src2Op
        _ -> error $ "Cannot parse AAsm instruction: " ++ instr

-- Process a binary operation, dispatching to specific handlers or emitBinary
processBinOp :: Operand -> Operand -> String -> Operand -> RegAlloc ()
processBinOp destOp src1Op opStr actualSrc2Op =
    case opStr of
        "+" -> emitBinary "add" destOp src1Op actualSrc2Op
        "-" -> emitBinary "sub" destOp src1Op actualSrc2Op
        "*" -> emitBinary "imul" destOp src1Op actualSrc2Op
        "/" -> do
            -- Handle division: destOp = src1Op / actualSrc2Op
            emit "\t# DEBUG: Division Start"
            emitMove (Reg "rax") src1Op -- Dividend must be in rax
            let divisorFinalOp = actualSrc2Op
            when (divisorFinalOp == Reg "rax" || divisorFinalOp == Reg "rdx") $
                error $
                    "Division divisor " ++ show divisorFinalOp ++ " conflicts with rax/rdx usage."
            emit "\t# DEBUG: Division Divisor OK"

            emit "\t# DEBUG: Before PUSH RDX for Division"
            emit "\tpush rdx" -- Save rdx (clobbered by idiv)
            emit "\t# DEBUG: Before CQO for Division"
            emit "\tcqo" -- sign-extend rax→rdx:rax in 64-bit mode
            emit "\t# DEBUG: Before IDIV for Division"
            emitDivIdiv divisorFinalOp -- idiv operand (quotient in rax, remainder in rdx)
            emit "\t# DEBUG: Before MOV DEST_OP, RAX for Division (Quotient)"
            emitMove destOp (Reg "rax") -- Move quotient (from rax) to its final destination
            emit "\t# DEBUG: Before POP RDX for Division"
            emit "\tpop rdx" -- Restore original rdx
            emit "\t# DEBUG: Division End"
        "%" -> do
            -- Handle modulus: destOp = src1Op % actualSrc2Op
            emit "\t# DEBUG: Modulo Start"
            emitMove (Reg "rax") src1Op -- Dividend must be in rax
            let divisorFinalOp = actualSrc2Op
            when (divisorFinalOp == Reg "rax" || divisorFinalOp == Reg "rdx") $
                error $
                    "Modulus divisor " ++ show divisorFinalOp ++ " conflicts with rax/rdx usage for idiv."
            emit "\t# DEBUG: Modulo Divisor OK"

            emit "\t# DEBUG: Before PUSH RDX for Modulus"
            emit "\tpush rdx" -- Save rdx
            emit "\t# DEBUG: Before CDQ for Modulus"
            emit "\tcdq" -- Sign extend rax into rdx:rax
            emit "\t# DEBUG: Before IDIV for Modulus"
            emitDivIdiv divisorFinalOp -- idiv operand (remainder in rdx)
            emit "\t# DEBUG: Before MOV DEST_OP, RDX for Modulus (Remainder)"
            emitMove destOp (Reg "rdx") -- Move remainder (from rdx) to its final destination
            emit "\t# DEBUG: Before POP RDX for Modulus"
            emit "\tpop rdx" -- Restore original rdx
            emit "\t# DEBUG: Modulo End"
        _ -> error $ "Unsupported binary AAsm operator: " ++ opStr

-- Emit a MOV instruction, handling various operand combinations
emitMove :: Operand -> Operand -> RegAlloc ()
emitMove dest@(Reg _) src@(Imm _) = emit' "mov" [dest, src]
emitMove dest@(Mem _) src@(Imm _) = emit' "mov" [dest, src]
emitMove dest@(Reg destReg) src@(Reg srcReg)
    | destReg == srcReg = emit ("\t# DEBUG: emitMove NOP for " ++ destReg ++ " <- " ++ srcReg) -- Optimized: mov reg, reg is a NOP
    | otherwise = emit' "mov" [dest, src]
emitMove dest@(Reg _) src@(Mem _) = emit' "mov" [dest, src]
emitMove dest@(Mem _) src@(Reg _) = emit' "mov" [dest, src]
emitMove dest@(Mem dm) src@(Mem sm) = do
    -- Memory-to-memory move requires a temporary register (rax)
    emit $ "\t# DEBUG: Mem to Mem Move START (" ++ dm ++ " <- " ++ sm ++ ")"
    emit "\tpush rax" -- Save rax (it's not in freeRegs, so this is mainly for very complex scenarios or future changes)
    emit' "mov" [Reg "rax", src] -- Load source memory to rax
    emit' "mov" [dest, Reg "rax"] -- Store rax to destination memory
    emit "\tpop rax" -- Restore original rax
    emit "\t# DEBUG: Mem to Mem Move END"
emitMove d s = error $ "Unsupported move combination: " ++ show d ++ " <- " ++ show s

-- Emit a unary operation (e.g., neg)
emitUnaryOp :: String -> Operand -> RegAlloc ()
emitUnaryOp op destOp = emit' op [destOp]

-- Emit a generic binary instruction (add, sub, imul), handling operand constraints
emitBinary :: String -> Operand -> Operand -> Operand -> RegAlloc ()
emitBinary opName destOp src1Op src2Op = do
    emit $ "\t# DEBUG: emitBinary " ++ opName ++ " for " ++ show destOp ++ " (" ++ show src1Op ++ " " ++ opName ++ " " ++ show src2Op ++ ")"

    if opName == "imul"
        then
            -- Special handling for IMUL:
            -- The two-operand 'imul reg, r/m' stores result in 'reg'.
            -- The three-operand 'imul reg, r/m, imm' stores result in 'reg'.
            -- We will use RAX as the register to perform the multiplication,
            -- then move the result to the final 'destOp'.
            do
                emit "\t# DEBUG: IMUL specific handling"
                emit "\tpush rax" -- Save current rax before using it as a scratch register
                emit "\tpush rdx" -- Save rdx. Two-operand 'imul r, r/m' does not clobber rdx.
                -- One-operand 'imul r/m' (which we are not generating here) does.
                -- Saving for safety/consistency for now.

                -- Step 1: Get src1Op's value into RAX.
                emitMove (Reg "rax") src1Op
                emit "\t# DEBUG: IMUL - rax now has src1Val"

                -- Step 2: Perform multiplication: RAX = RAX * src2Op
                -- We use the two-operand 'imul rax, r/m' or 'imul rax, imm'.
                case src2Op of
                    Imm _ -> emit' "imul" [Reg "rax", src2Op] -- imul rax, imm
                    Reg _ -> emit' "imul" [Reg "rax", src2Op] -- imul rax, reg
                    Mem _ -> emit' "imul" [Reg "rax", src2Op] -- imul rax, mem
                emit "\t# DEBUG: IMUL - rax now has result (src1Val * src2Val)"

                -- Step 3: Move result from RAX to the final destOp
                emitMove destOp (Reg "rax")
                emit "\t# DEBUG: IMUL - result moved to final destOp"

                emit "\tpop rdx" -- Restore original rdx
                emit "\tpop rax" -- Restore original rax
        else
            -- Generic handling for other ops like add, sub.
            -- These instructions (add, sub) are more flexible with memory destinations:
            -- add r/m, r   OR   add r/m, imm
            -- The strategy is: ensure destOp holds src1Val, then perform "opName destOp, src2Op".
            do
                unless (destOp == src1Op) $ do
                    emit $ "\t# DEBUG: emitBinary (" ++ opName ++ ") - destOp is different from src1Op, doing initial move: " ++ show destOp ++ " <- " ++ show src1Op
                    emitMove destOp src1Op

                emit $ "\t# DEBUG: emitBinary (" ++ opName ++ ") - after initial move (if any), destOp (" ++ show destOp ++ ") now holds src1Val. Operating with src2Op (" ++ show src2Op ++ ")"

                case (destOp, src2Op) of
                    (Reg _, Reg _) -> emit' opName [destOp, src2Op]
                    (Reg _, Mem _) -> emit' opName [destOp, src2Op]
                    (Mem _, Reg _) -> emit' opName [destOp, src2Op]
                    (Mem _, Mem _) -> do
                        emit $ "\t# DEBUG: emitBinary (" ++ opName ++ ") Mem, Mem op"
                        emit "\tpush rax"
                        emitMove (Reg "rax") src2Op
                        emit' opName [destOp, Reg "rax"]
                        emit "\tpop rax"
                    (Mem _, Imm _) -> emit' opName [destOp, src2Op]
                    (Reg _, Imm _) -> emit' opName [destOp, src2Op]
                    _ -> error $ "Unsupported binary operand combination for " ++ opName ++ ": " ++ show destOp ++ ", " ++ show src2Op

-- Emit the IDIV instruction (operand is the divisor)
emitDivIdiv :: Operand -> RegAlloc ()
emitDivIdiv divisorOp = emit' "idiv" [divisorOp]

-- Helper to trim leading/trailing whitespace
trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse