{-# LANGUAGE InstanceSigs #-}

module Compile.X86 (
    X86Program (..),
    emitProgram,
    allocatableRegisters,
    callerSavedRegisters,
    calleeSavedRegisters,
    rax,
    rbx,
    rcx,
    rdx,
    rsp,
    rbp,
    rsi,
    rdi,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,
    regToReg,
    immToReg,
    push,
    pop,
    clearReg,
    Size (..),
    Register (..),
    Operand (..),
    Instruction (..),
    Label,
) where

data Size = B | W | L | Q
    deriving (Show, Eq)

data Register
    = RAX
    | RBX
    | RCX
    | RDX
    | RSP
    | RBP
    | RSI
    | RDI
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    deriving (Eq, Ord, Enum)

type Label = String

sizedRegister :: Register -> Size -> String
sizedRegister reg size = "%" ++ prefix ++ suffix
  where
    suffix = case reg of
        RAX -> "ax"
        RBX -> "bx"
        RCX -> "cx"
        RDX -> "dx"
        RSP -> "sp"
        RBP -> "bp"
        RSI -> "si"
        RDI -> "di"
        R8 -> "8"
        R9 -> "9"
        R10 -> "10"
        R11 -> "11"
        R12 -> "12"
        R13 -> "13"
        R14 -> "14"
        R15 -> "15"

    prefix = case size of
        B -> if reg `elem` [RAX, RBX, RCX, RDX] then "" else "r"
        W -> ""
        L -> "e"
        Q -> "r"

instance Show Register where
    show :: Register -> String
    show reg = sizedRegister reg Q

data Operand
    = Reg Register
    | Imm Int
    | Mem Register (Maybe Int)
    | LabelOp Label
    deriving (Eq)

instance Show Operand where
    show :: Operand -> String
    show (Reg r) = show r
    show (Imm i) = "$" ++ show i
    show (Mem r Nothing) = "(" ++ show r ++ ")"
    show (Mem r (Just offset)) = show offset ++ "(" ++ show r ++ ")"
    show (LabelOp l) = l

data Instruction
    = MovQ Operand Operand
    | PushQ Operand
    | PopQ Operand
    | AddQ Operand Operand
    | SubQ Operand Operand
    | ImulQ Operand Operand
    | IdivQ Operand
    | Cqto
    | NegQ Operand
    | XorQ Operand Operand
    | CmpQ Operand Operand
    | Jmp Label
    | JmpInd Operand
    | Je Label
    | Jne Label
    | Jg Label
    | Jge Label
    | Jl Label
    | Jle Label
    | Label Label
    | Call Label
    | Ret
    | Comment String
    | ModRmQ Operand Operand
    deriving (Eq)

instance Show Instruction where
    show :: Instruction -> String
    show (MovQ src dest) = "movq " ++ show src ++ ", " ++ show dest
    show (PushQ op) = "pushq " ++ show op
    show (PopQ op) = "popq " ++ show op
    show (AddQ src dest) = "addq " ++ show src ++ ", " ++ show dest
    show (SubQ src dest) = "subq " ++ show src ++ ", " ++ show dest
    show (ImulQ src dest) = "imulq " ++ show src ++ ", " ++ show dest
    show (IdivQ op) = "idivq " ++ show op
    show Cqto = "cqto"
    show (NegQ op) = "negq " ++ show op
    show (XorQ src dest) = "xorq " ++ show src ++ ", " ++ show dest
    show (CmpQ src dest) = "cmpq " ++ show src ++ ", " ++ show dest
    show (Jmp label) = "jmp " ++ label
    show (JmpInd op) = "jmp *" ++ show op
    show (Je label) = "je " ++ label
    show (Jne label) = "jne " ++ label
    show (Jg label) = "jg " ++ label
    show (Jge label) = "jge " ++ label
    show (Jl label) = "jl " ++ label
    show (Jle label) = "jle " ++ label
    show (Label label) = label ++ ":"
    show (Call label) = "call " ++ label
    show Ret = "ret"
    show (Comment str) = "# " ++ str
    show (ModRmQ src dest) = "??? " ++ show src ++ ", " ++ show dest

data X86Program = X86Program
    { globals :: [String]
    , text :: [Instruction]
    }
    deriving (Show)

emitATT :: X86Program -> String
emitATT prog =
    unlines
        [ ".global main"
        , ".global _main"
        , ""
        , ".text"
        , "main:"
        , "call _main"
        , "    pushq %rbp"
        , "    movq %rsp, %rbp"
        , "    call _main"
        , "    movq %rax, %rdi"
        , "    movq $60, %rax"
        , "    syscall"
        , ""
        , "_main:" -- Add an explicit _main label for the entry point
        , unlines (map show (text prog))
        ]

-- emitIntel :: X86Program -> String
-- emitIntel prog =
--     unlines
--         [ ".intel_syntax noprefix"
--         , ".global main"
--         , ".global _main"
--         , ""
--         , ".text"
--         , "main:"
--         , "    push rbp"
--         , "    mov rbp, rsp"
--         , "    call _main"
--         , "    mov rdi, rax"
--         , "    mov rax, 60"
--         , "    syscall"
--         , ""
--         , "_main:" -- Add an explicit _main label for the entry point
--         , unlines (map (showIntel . convertToIntel) (text prog))
--         ]
--   where
--     convertToIntel = id
--     showIntel = show

emitProgram :: X86Program -> String
emitProgram = emitATT

callerSavedRegisters :: [Register]
callerSavedRegisters = [RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11]

calleeSavedRegisters :: [Register]
calleeSavedRegisters = [RBX, R12, R13, R14, R15]

allocatableRegisters :: [Register]
allocatableRegisters =
    [ RAX
    , RCX
    , RDX
    , R10
    , R11
    , RBX
    , R12
    , R13
    , R14
    , R15
    , RSI
    , RDI
    , R8
    , R9
    ]

rax, rbx, rcx, rdx, rsp, rbp, rsi, rdi :: Register
rax = RAX
rbx = RBX
rcx = RCX
rdx = RDX
rsp = RSP
rbp = RBP
rsi = RSI
rdi = RDI

r8, r9, r10, r11, r12, r13, r14, r15 :: Register
r8 = R8
r9 = R9
r10 = R10
r11 = R11
r12 = R12
r13 = R13
r14 = R14
r15 = R15

regToReg :: Register -> Operand
regToReg = Reg

immToReg :: Int -> Register -> Instruction
immToReg i r = MovQ (Imm i) (Reg r)

push :: Register -> Instruction
push = PushQ . Reg

pop :: Register -> Instruction
pop = PopQ . Reg

clearReg :: Register -> Instruction
clearReg r = XorQ (Reg r) (Reg r)